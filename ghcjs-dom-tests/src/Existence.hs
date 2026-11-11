{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Existence check for every generated binding (in the spirit of WPT's
-- idlharness, but only "is it there"): each interface object and member
-- the bindings use is looked up in the running browser and compared with
-- what MDN's browser-compat-data says this engine ships.
--
-- A binding BCD says this engine supports but which the browser lacks is a
-- failure (wrong owner, misspelt name, stale data).  The other mismatches
-- are only counted: present but not in BCD is common (BCD lags), absent
-- and not expected is what the BCD filter allows (two other engines).
module Existence (existenceCheck) where

import Data.Text (Text)
import qualified Data.Text as T

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM.NavigatorID as NavigatorID
import qualified GHCJS.DOM.Window as Window

import Existence.Spec (existenceSpec)

#if defined(javascript_HOST_ARCH)
import Control.Monad.IO.Class (liftIO)
import Data.JSString.Text (textFromJSString, textToJSString)
import GHCJS.Types (JSString)

foreign import javascript unsafe "((s, a) => (0, eval)(s)(a))"
  js_apply :: JSString -> JSString -> IO JSString

applyJS :: Text -> Text -> JSM Text
applyJS f a = liftIO (textFromJSString <$> js_apply (textToJSString f) (textToJSString a))
#else
import Language.Javascript.JSaddle (call, eval, fromJSValUnchecked, global, toJSVal)

applyJS :: Text -> Text -> JSM Text
applyJS f a = do
  fn <- eval ("(" <> f <> ")")
  arg <- toJSVal a
  call fn global [arg] >>= fromJSValUnchecked
#endif

-- | Takes the spec lines joined by newlines; returns one character per line:
-- @1@ present, @0@ absent, @-@ nowhere to look (e.g. the owner has no
-- interface object, like the WebGL extension interfaces).
lookupAll :: Text
lookupAll = T.unwords
  [ "(spec) => {"
  -- [LegacyUnforgeable] members live on instances, not prototypes
  , "  const inst = {Document: document, HTMLDocument: document, Location: location,"
  , "    Navigator: navigator, Screen: screen, History: history, Event: new Event('x')};"
  , "  const where = (o) => {"
  -- [Global]: members are on the global object itself
  , "    if (o === 'Window') return [globalThis];"
  , "    const c = globalThis[o], r = [];"
  , "    if (typeof c === 'function' && c.prototype) r.push(c.prototype);"
  , "    if (inst[o]) r.push(inst[o]);"
  , "    return r; };"
  , "  return spec.split('\\n').map(l => {"
  , "    const [k, os, n] = l.split('\\t');"
  , "    if (k === 'I') return (n in globalThis) ? '1' : '0';"
  , "    let found = false, any = false;"
  , "    for (const o of os.split('|')) {"
  , "      const objs = k === 'S' ? (globalThis[o] != null ? [globalThis[o]] : []) : where(o);"
  , "      for (const x of objs) { any = true; if (n in Object(x)) found = true; } }"
  , "    return any ? (found ? '1' : '0') : '-';"
  , "  }).join(''); }"
  ]

-- | The BCD engine letter for the running browser and its version (empty
-- when the user agent does not say: then every BCD version counts).
browserEngine :: JSM (Char, [Int])
browserEngine = do
  ua <- currentWindowUnchecked >>= Window.getNavigator >>= NavigatorID.getUserAgent
  let after p = case T.breakOn p ua of
        (_, r) | not (T.null r) -> versionKey (T.takeWhile (\c -> c /= ' ' && c /= ';') (T.drop (T.length p) r))
        _ -> []
  pure $ if | "Firefox/" `T.isInfixOf` ua -> ('g', after "Firefox/")
            | "Chrome/" `T.isInfixOf` ua -> ('c', after "Chrome/")
            | otherwise -> ('w', after "Version/")

versionKey :: Text -> [Int]
versionKey = map (\s -> case reads (T.unpack s) of [(n, "")] -> n; _ -> 0) . T.splitOn "."

-- | Interfaces BCD lists for an engine that the engine ships without an
-- interface object (the bindings work on instances, but @castTo@ and
-- @gType*@ need the global).  Keyed by engine letter.
noInterfaceObject :: [(Char, [Text])]
noInterfaceObject =
  [ ('c', [ -- File and Directory Entries API: Chrome only has the webkit* entry points
            "FileSystem", "FileSystemDirectoryEntry", "FileSystemDirectoryReader"
          , "FileSystemEntry", "FileSystemFileEntry"
            -- Web Speech API result objects
          , "SpeechRecognitionAlternative", "SpeechRecognitionResult", "SpeechRecognitionResultList" ])
  , ('g', [ -- devicemotion's sub-objects and window.external ([object External])
            "DeviceMotionEventAcceleration", "DeviceMotionEventRotationRate", "External" ])
  ]

-- | Members BCD lists for an engine that it only exposes in some contexts
-- (never to an ordinary page).  Keyed by engine letter.
contextOnly :: [(Char, [Text])]
contextOnly =
  [ ('w', [ -- Badging API: installed web apps only ("Add to Dock")
            "Navigator.setAppBadge", "Navigator.clearAppBadge" ])
  ]
-- (Desktop Firefox also hides TouchEvent unless dom.w3c_touch_events.enabled
-- is 1 or it detects a touch screen; the test profile sets the pref.)

-- | Partial: BCD lists only a partial implementation, which may be missing
-- from some builds (platform limited, behind a pref, needs a touch screen).
data Expect = Expected | Partial | Later | NotListed | Unknown deriving Eq

-- | Fails on bindings BCD says this engine (at this version) has but the
-- browser lacks.  The report goes to stdout.
existenceCheck :: (Text -> JSM ()) -> JSM (Either Text ())
existenceCheck report = do
  (e, ver) <- browserEngine
  rs <- applyJS lookupAll (T.intercalate "\n" existenceSpec)
  let rows = [ (fields l, r) | (l, r) <- zip existenceSpec (T.unpack rs) ]
      fields l = case T.splitOn "\t" l of
        [k, os, n, es] -> (k, os, n, es)
        _ -> ("", "", l, "")
      known = concat [ ns | (k, ns) <- noInterfaceObject, k == e ]
      expect (_, _, _, "?") = Unknown
      expect (_, _, _, es) = case [ T.tail t | t <- T.words es, T.head t == e ] of
        [] -> NotListed
        (v0 : _) | let v = T.dropWhile (== '~') v0, T.null v || null ver || versionKey v <= ver ->
                     if "~" `T.isPrefixOf` v0 then Partial else Expected
                | otherwise -> Later
      count p = length (filter p rows)
      context = concat [ ns | (k, ns) <- contextOnly, k == e ]
      isKnown ((k, _, n, _), _) = k == "I" && n `elem` known
      isContext ((k, os, n, _), _) = k /= "I" && (os <> "." <> n) `elem` context
      missing = [ f | (f, '0') <- rows, expect f == Expected, not (isKnown (f, '0')), not (isContext (f, '0')) ]
      describe (k, os, n, es) = (if k == "I" then n else os <> "." <> n) <> " [" <> es <> "]"
  mapM_ (report . ("  missing: " <>) . describe) (take 60 missing)
  report $ "  existence (engine " <> T.singleton e <> " " <> T.intercalate "." (map tshow ver) <> "): "
    <> tshow (length rows) <> " checks, "
    <> tshow (count ((== '1') . snd)) <> " present, "
    <> tshow (length missing) <> " missing though BCD lists them, "
    <> tshow (count (\r -> isKnown r && snd r == '0')) <> " known to have no interface object, "
    <> tshow (count (\r -> isContext r && snd r == '0')) <> " absent and only exposed in some contexts, "
    <> tshow (count (\(f, r) -> r == '0' && expect f == Partial)) <> " absent where BCD says partial, "
    <> tshow (count (\(f, r) -> r == '0' && expect f == Later)) <> " absent and BCD says a later version, "
    <> tshow (count (\(f, r) -> r == '1' && expect f `elem` [NotListed, Later])) <> " present though BCD does not list them, "
    <> tshow (count (\(f, r) -> r == '0' && expect f `elem` [NotListed, Unknown])) <> " absent (not in BCD for this engine), "
    <> tshow (count ((== '-') . snd)) <> " without an interface object to look on"
  pure $ if length rows /= T.length rs
    then Left ("result length " <> tshow (T.length rs) <> " for " <> tshow (length rows) <> " checks")
    else if null missing then Right () else Left (tshow (length missing) <> " bindings missing (see above)")
  where tshow :: Show a => a -> Text
        tshow = T.pack . show
