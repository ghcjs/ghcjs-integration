# Pinned, JSON-only inputs for dom-gen.  No JavaScript is run: webref
# already publishes its curated IDL parsed to JSON (webidl2 AST) in each
# release commit, and MDN browser-compat-data is a single data.json.
#
#   nix build .#dom-gen-inputs -o dom-gen/inputs
#
# Result layout:
#   index.json              spec index (dom-gen keeps specs in the "browser"
#                           category, as webref's @webref/idl package does)
#   idlparsed/<spec>.json   curated IDL, parsed (ed/idlparsed of the release)
#   events.json             @webref/events data
#   elements/<spec>.json    @webref/elements data
#   bcd.json                @mdn/browser-compat-data data.json
#   versions.json           what was pinned (embedded in generated headers)
#
# Bump all webref data together by changing `webref.rev` to the commit of a
# newer `@webref/idl@X.Y.Z` tag (git ls-remote https://github.com/w3c/webref
# 'refs/tags/@webref/idl@*'); events/elements come from the same commit.
{ lib, fetchgit, fetchurl, runCommand, writeText }:
let
  webref = {
    tag = "@webref/idl@3.86.0";
    rev = "bb927df3d6cc86f1c0beca204844a170a0843a18";
    hash = "sha256-qMpP+zU+mQXcXTOVHhy6pz3JbcjMFVmTMUKYkwETYKw=";
  };
  bcd = {
    version = "8.1.8";
    hash = "sha512-+RK7inkLeZ70kr9Mo+Y81JjZMpQIt0Sx40tHHU0hXGDSdf9YvlawH+4WlUpP3SE8rFLmjqGiTzL+qPPPnncWDw==";
  };

  webrefSrc = fetchgit {
    url = "https://github.com/w3c/webref.git";
    inherit (webref) rev hash;
    sparseCheckout = [ "ed/index.json" "ed/idlparsed" "ed/events.json" "ed/elements" ];
  };
  bcdTarball = fetchurl {
    url = "https://registry.npmjs.org/@mdn/browser-compat-data/-/browser-compat-data-${bcd.version}.tgz";
    inherit (bcd) hash;
  };
  versions = writeText "versions.json" (builtins.toJSON {
    webref = { inherit (webref) tag rev; };
    browser-compat-data = bcd.version;
  });
in runCommand "dom-gen-inputs-${lib.removePrefix "@webref/idl@" webref.tag}" { } ''
  mkdir -p $out
  cp ${webrefSrc}/ed/index.json $out/index.json
  cp -r ${webrefSrc}/ed/idlparsed $out/idlparsed
  cp ${webrefSrc}/ed/events.json $out/events.json
  cp -r ${webrefSrc}/ed/elements $out/elements
  tar -xzf ${bcdTarball} -O package/data.json > $out/bcd.json
  cp ${versions} $out/versions.json
''
