#!/usr/bin/env bash
# Run the ghcjs-dom browser tests and print the results (headless, except
# Safari: it has no headless mode).
#
#   ghcjs-dom-tests/run-browser-tests.sh BROWSER BACKEND
#
#   BROWSER  chrome | firefox | safari   (the apps in /Applications; safari
#            needs `safaridriver --enable` once)
#   BACKEND  js | native
#
# Run from the repository root after building:
#   js:     nix develop '.?submodules=1#javascript-unknown-ghcjs' -c \
#             cabal build --project-file=cabal.project.js ghcjs-dom-tests
#   native: nix develop '.?submodules=1' -c cabal build ghcjs-dom-tests
#
# Exits 0 when the DONE line reports every check passed.  Uses throwaway
# browser profiles under dist-newstyle/browser-tests (never your own).
set -euo pipefail

browser=${1:?usage: $0 chrome|firefox|safari js|native}
backend=${2:?usage: $0 chrome|firefox|safari js|native}
wait_secs=${WAIT_SECS:-30}

root=$PWD
work=$root/dist-newstyle/browser-tests
mkdir -p "$work"
log=$work/$browser-$backend.log
: > "$log"

pids=()
cleanup() { for p in "${pids[@]}"; do kill "$p" 2>/dev/null || true; done; }
trap cleanup EXIT

case $backend in
  js)
    jsexe=$(echo dist-newstyle/build/javascript-ghcjs/ghc-*/ghcjs-dom-tests-*/x/ghcjs-dom-tests/build/ghcjs-dom-tests/ghcjs-dom-tests.jsexe)
    [ -d "$jsexe" ] || { echo "no $jsexe: build the js backend first" >&2; exit 2; }
    # The generated index.html has no charset, and the wasm embedded in the
    # JS is a raw byte string, so the page must declare UTF-8.
    printf '<!DOCTYPE html>\n<html><head><meta charset="utf-8"></head><body></body><script src="all.js" defer></script></html>\n' > "$jsexe/utf8.html"
    port=${PORT:-8199}
    darkhttpd=$(nix build --no-link --print-out-paths nixpkgs#darkhttpd)/bin/darkhttpd
    "$darkhttpd" "$jsexe" --addr 127.0.0.1 --port "$port" > "$work/darkhttpd.log" 2>&1 &
    pids+=($!)
    url=http://127.0.0.1:$port/utf8.html
    ;;
  native)
    exe=$(nix develop '.?submodules=1' -c cabal list-bin ghcjs-dom-tests 2>/dev/null | tail -1)
    [ -x "$exe" ] || { echo "no $exe: build the native backend first" >&2; exit 2; }
    # jsaddle-warp on port 3709; the results are printed by the server
    "$exe" > "$log" 2>&1 &
    pids+=($!)
    url=http://127.0.0.1:3709/
    ;;
  *) echo "unknown backend $backend" >&2; exit 2 ;;
esac
sleep 2

# Browser output: on the js backend the results come from console.log.
blog=$work/$browser-$backend.browser.log
case $browser in
  chrome)
    profile=$work/chrome-profile
    mkdir -p "$profile"
    # (--dump-dom/--virtual-time-budget would stall: virtual time stops the
    # Promise and requestAnimationFrame checks, so run in real time.)
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new --disable-gpu \
      --no-first-run --user-data-dir="$profile" --enable-logging=stderr --v=0 \
      --remote-debugging-port=0 "$url" > /dev/null 2> "$blog" &
    pids+=($!)
    ;;
  firefox)
    profile=$work/firefox-profile
    mkdir -p "$profile"
    cat > "$profile/user.js" <<'EOF'
user_pref("devtools.console.stdout.content", true);
user_pref("dom.w3c_touch_events.enabled", 1);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);
user_pref("browser.aboutwelcome.enabled", false);
user_pref("app.update.enabled", false);
EOF
    # -no-remote: never talk to an already running Firefox
    /Applications/Firefox.app/Contents/MacOS/firefox --headless -no-remote -profile "$profile" "$url" > "$blog" 2>&1 &
    pids+=($!)
    ;;
  safari)
    # No headless Safari: safaridriver opens a separate automation window
    # (its own session; needs `safaridriver --enable` once).
    sport=${SAFARIDRIVER_PORT:-8198}
    safaridriver -p "$sport" > "$blog" 2>&1 &
    pids+=($!)
    jq=$(nix build --no-link --print-out-paths nixpkgs#jq.bin)/bin/jq
    wd="http://127.0.0.1:$sport"
    for _ in $(seq 20); do curl -sf "$wd/status" > /dev/null && break; sleep 0.5; done
    session=$(curl -sf -X POST "$wd/session" -H 'Content-Type: application/json' \
      -d '{"capabilities":{"alwaysMatch":{"browserName":"safari"}}}' | "$jq" -r .value.sessionId)
    [ -n "$session" ] && [ "$session" != null ] || { echo "could not start a Safari session (see $blog)" >&2; exit 1; }
    cleanup() {
      curl -s -X DELETE "$wd/session/$session" > /dev/null || true
      for p in "${pids[@]}"; do kill "$p" 2>/dev/null || true; done
    }
    curl -sf -X POST "$wd/session/$session/url" -H 'Content-Type: application/json' \
      -d "$("$jq" -nc --arg u "$url" '{url: $u}')" > /dev/null
    ;;
  *) echo "unknown browser $browser" >&2; exit 2 ;;
esac

# Wait for the DONE line.
results() {
  if [ "$backend" = native ]; then
    cat "$log"
  elif [ "$browser" = safari ]; then
    curl -s -X POST "$wd/session/$session/execute/sync" -H 'Content-Type: application/json' \
      -d '{"script":"var r = document.getElementById(\"results\"); return r ? r.textContent : \"\";","args":[]}' \
      | "$jq" -r '.value // empty' 2>/dev/null || true
  elif [ "$browser" = chrome ]; then
    # one console message may span several lines: CONSOLE...] "text...", source: ...
    sed -n '/CONSOLE:[0-9]*\] "/,/", source: /{s/.*CONSOLE:[0-9]*\] "//; s/", source: .*//; p;}' "$blog"
  else
    grep -a '^console.log: ' "$blog" | sed 's/^console.log: "//; s/"$//; s/\\n/\n/g'
  fi
}
for _ in $(seq "$wait_secs"); do
  results | grep -q '^DONE ' && break
  sleep 1
done

results | grep -E '^(PASS|FAIL|DONE|  )' || true
done_line=$(results | grep '^DONE ' | tail -1)
[ -n "$done_line" ] || { echo "no DONE line after ${wait_secs}s (logs in $work)" >&2; exit 1; }
n=${done_line#DONE }
[ "${n%/*}" = "${n#*/}" ]
