-- | Load the pinned inputs produced by @dom-gen/inputs.nix@ and assemble the
-- 'Dom' model:
--
--   * merge @partial@ definitions into their base definition
--   * keep mixins as separate definitions, recording @includes@ both ways
--     (mixins become @Is*@ classes, as in the old bindings)
--   * attach MDN browser-compat-data support to definitions and members
--   * attach @webref/events and @webref/elements data
--
-- Nothing is filtered here; inclusion policy is in "DomGen.Env".
module DomGen.Load
  ( loadDom
  ) where

import Control.Monad (foldM, forM, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import DomGen.Model

decodeFile :: A.FromJSON a => FilePath -> IO a
decodeFile path = BL.readFile path >>= either (fail . ((path <> ": ") <>)) pure . A.eitherDecode

jsonFiles :: FilePath -> IO [FilePath]
jsonFiles dir = sort . filter ((== ".json") . takeExtension) <$> listDirectory dir

-- | @loadDom inputsDir@
loadDom :: FilePath -> IO Dom
loadDom dir = do
  versions <- decodeFile (dir </> "versions.json")
  -- Like webref's @webref/idl package, only use specs in the "browser"
  -- category (this drops e.g. JSON-LD and EPUB IDL).
  index <- decodeFile (dir </> "index.json") :: IO A.Value
  specFiles <- either fail pure $ flip A.parseEither index $ A.withObject "index" $ \o -> do
    results <- o A..: "results" :: A.Parser [A.Object]
    fmap concat . forM results $ \r -> do
      cats <- r A..:? "categories" A..!= []
      file <- r A..:? "idlparsed"
      short <- r A..: "shortname"
      pure [ (short, f) | Just f <- [file], ("browser" :: Text) `elem` cats ]
  specs <- forM (sortOn fst specFiles) $ \(short, f) -> do
    ip <- decodeFile (dir </> f)
    pure (short, ip)

  -- Main (non-partial) definitions
  let mains = [ (spec, d) | (spec, ip) <- specs, d <- M.elems (ipNames ip) ]
  st0 <- foldM addMain emptyAcc mains

  -- Partials and includes, in spec order (deterministic)
  let extended = [ (spec, d) | (spec, ip) <- specs, ds <- M.elems (ipExtended ip), d <- ds ]
  st1 <- foldM addExtended st0 extended

  let defs1 = linkIncludes (accDefs st1) (accIncludes st1)
  bcd <- decodeFile (dir </> "bcd.json")
  -- Force every BCD lookup now so the (large) decoded bcd.json is not kept
  -- alive by unevaluated thunks for the rest of the run.
  let defs2 = forceBcd (M.map (attachBcd bcd defs1) defs1)
  defs2 `seq` pure ()

  events <- decodeFile (dir </> "events.json")
  elemFiles <- jsonFiles (dir </> "elements")
  elements <- fmap concat . forM elemFiles $ \f -> do
    v <- decodeFile (dir </> "elements" </> f) :: IO A.Value
    either fail pure $ flip A.parseEither v $ A.withObject "elements" (A..: "elements")

  pure Dom
    { domVersions = versions
    , domDefinitions = defs2
    , domEnums = accEnums st1
    , domTypedefs = accTypedefs st1
    , domCallbacks = accCallbacks st1
    , domEvents = events
    , domElements = elements
    }

------------------------------------------------------------------------------

data Acc = Acc
  { accDefs :: M.Map Text Definition
  , accEnums :: M.Map Text EnumDef
  , accTypedefs :: M.Map Text Typedef
  , accCallbacks :: M.Map Text CallbackDef
  , accIncludes :: [(Text, Text)]
  }

emptyAcc :: Acc
emptyAcc = Acc mempty mempty mempty mempty []

insertNew :: Text -> Text -> M.Map Text a -> a -> IO (M.Map Text a)
insertNew spec name m x = do
  when (M.member name m) $ fail ("duplicate definition " <> T.unpack name <> " in " <> T.unpack spec)
  pure (M.insert name x m)

addMain :: Acc -> (Text, RawDef) -> IO Acc
addMain acc (spec, raw) = case raw of
  RawDefinition d _ -> do
    let d' = d { defSpec = spec, defMembers = map (\m -> m { memSpec = spec }) (defMembers d) }
    m <- insertNew spec (defName d) (accDefs acc) d'
    pure acc { accDefs = m }
  RawEnum e -> (\m -> acc { accEnums = m }) <$> insertNew spec (enumName e) (accEnums acc) e { enumSpec = spec }
  RawTypedef t -> (\m -> acc { accTypedefs = m }) <$> insertNew spec (typedefName t) (accTypedefs acc) t { typedefSpec = spec }
  RawCallback c -> (\m -> acc { accCallbacks = m }) <$> insertNew spec (cbName c) (accCallbacks acc) c { cbSpec = spec }
  RawIncludes t i -> pure acc { accIncludes = accIncludes acc <> [(t, i)] }

addExtended :: Acc -> (Text, RawDef) -> IO Acc
addExtended acc (spec, raw) = case raw of
  RawIncludes t i -> pure acc { accIncludes = accIncludes acc <> [(t, i)] }
  RawDefinition p True -> case M.lookup (defName p) (accDefs acc) of
    Nothing -> fail ("partial " <> T.unpack (defName p) <> " (" <> T.unpack spec <> ") has no base definition")
    Just base -> do
      unless (defKind base == defKind p) $ fail ("partial kind mismatch for " <> T.unpack (defName p))
      let ms = map (\m -> m { memSpec = spec }) (defMembers p)
      pure acc { accDefs = M.insert (defName p) base { defMembers = defMembers base <> ms } (accDefs acc) }
  -- idlExtendedNames only holds partials and includes statements
  _ -> pure acc

linkIncludes :: M.Map Text Definition -> [(Text, Text)] -> M.Map Text Definition
linkIncludes defs incs = M.map link defs
  where
    byTarget = M.fromListWith (<>) [ (t, [i]) | (t, i) <- incs ]
    byMixin = M.fromListWith (<>) [ (i, [t]) | (t, i) <- incs ]
    link d = d
      { defIncludes = sort (nub (M.findWithDefault [] (defName d) byTarget))
      , defIncludedBy = sort (nub (M.findWithDefault [] (defName d) byMixin))
      }

------------------------------------------------------------------------------
-- MDN browser-compat-data

engines :: [(Text, K.Key)]
engines = [("chromium", "chrome"), ("gecko", "firefox"), ("webkit", "safari")]

lookupPath :: [Text] -> A.Value -> Maybe A.Value
lookupPath [] v = Just v
lookupPath (k : ks) (A.Object o) = KM.lookup (K.fromText k) o >>= lookupPath ks
lookupPath _ _ = Nothing

-- | A support statement counts when it is a stable, unprefixed, unflagged
-- and not removed implementation.  Partial implementations count: BCD uses
-- @partial_implementation@ for "exists, with caveats" (WebGPU only on some
-- platforms, touch events only with a touch screen, missing options), and
-- the browser does expose them (though, as for platform limited ones, not
-- necessarily in every build: those engines are listed in 'cPartial').
-- Gives whether the support is only partial and the earliest such
-- statement's @version_added@ (@""@ when BCD only says @true@; a leading
-- @≤@ is dropped), preferring complete statements.
stableSince :: A.Value -> Maybe (Bool, Text)
stableSince = \case
  A.Array a -> case mapMaybe stableSince (V.toList a) of
    [] -> Nothing
    vs -> let complete = [ v | (False, v) <- vs ]
          in Just $ if null complete then (True, minimumOn versionKey (map snd vs)) else (False, minimumOn versionKey complete)
  A.Object o ->
    let has k = maybe False truthy (KM.lookup k o)
        truthy = \case
          A.Null -> False
          A.Bool b -> b
          _ -> True
        added = case KM.lookup "version_added" o of
          Just (A.String s) | s /= "preview" -> Just (T.dropWhile (== '≤') s)
          Just (A.Bool True) -> Just ""
          _ -> Nothing
    in if any has ["version_removed", "flags", "prefix", "alternative_name"]
         then Nothing else (has "partial_implementation",) <$> added
  _ -> Nothing
  where minimumOn k = foldr1 (\a b -> if k a <= k b then a else b)

-- | Numeric comparison key for BCD versions ("17.4" < "100").
versionKey :: Text -> [Int]
versionKey = map (\s -> case reads (T.unpack s) of [(n, "")] -> n; _ -> 0) . T.splitOn "."

compatOf :: A.Value -> Maybe Compat
compatOf v = do
  A.Object c <- lookupPath ["__compat"] v
  A.Object support <- KM.lookup "support" c
  let status k = case lookupPath ["status", k] (A.Object c) of
        Just (A.Bool b) -> b
        _ -> False
      since = [ (e, s) | (e, b) <- engines, Just s <- [KM.lookup b support >>= stableSince] ]
  pure Compat
    { cEngines = map fst since
    , cSince = [ (e, s) | (e, (_, s)) <- since ]
    , cPartial = [ e | (e, (True, _)) <- since ]
    , cDeprecated = status "deprecated"
    , cExperimental = status "experimental"
    , cNonStandard = case lookupPath ["status", "standard_track"] (A.Object c) of
        Just (A.Bool False) -> True
        _ -> False
    , cMdn = case KM.lookup "mdn_url" c of
        Just (A.String u) -> Just u
        _ -> Nothing
    }

-- | Fully evaluate the BCD data attached to the definitions.
forceBcd :: M.Map Text Definition -> M.Map Text Definition
forceBcd defs = M.foldr (\d acc -> forceDef d `seq` acc) () defs `seq` defs
  where
    forceDef d = foldr (\m acc -> forceCompat (memBcd m) `seq` acc) (forceCompat (defBcd d)) (defMembers d)
    forceCompat = maybe () (\c -> foldr seq () (cEngines c) `seq` foldr seq () (cPartial c) `seq` foldr (\(e, s) acc -> e `seq` s `seq` acc) () (cSince c)
                                    `seq` maybe () (`seq` ()) (cMdn c))

mergeCompat :: Maybe Compat -> Maybe Compat -> Maybe Compat
mergeCompat Nothing b = b
mergeCompat a Nothing = a
mergeCompat (Just a) (Just b) = Just Compat
  { cEngines = sort (nub (cEngines a <> cEngines b))
  , cSince = M.toList (M.fromListWith (\x y -> if versionKey x <= versionKey y then x else y) (cSince a <> cSince b))
    -- partial unless one of them has complete support
  , cPartial = [ e | e <- sort (nub (cEngines a <> cEngines b)), not (complete a e), not (complete b e) ]
  , cDeprecated = cDeprecated a && cDeprecated b
  , cExperimental = cExperimental a && cExperimental b
  , cNonStandard = cNonStandard a && cNonStandard b
  , cMdn = maybe (cMdn b) Just (cMdn a)
  }
  where complete c e = e `elem` cEngines c && e `notElem` cPartial c

attachBcd :: A.Value -> M.Map Text Definition -> Definition -> Definition
attachBcd bcd defs d
  | defKind d `elem` [KDictionary, KCallbackInterface] = d
  | otherwise = d
      { defBcd = if defKind d == KMixin then Nothing else lookupPath ["api", defName d] bcd >>= compatOf
      , defMembers = map member (defMembers d)
      }
  where
    -- BCD records mixin members under each including interface, and
    -- sometimes records an inherited member (typically @foo_event@) only on
    -- the ancestor that first exposes it.
    owners = if defKind d == KMixin then defIncludedBy d else [defName d]
    ancestors o = o : maybe [] ancestors (M.lookup o defs >>= defInheritance)
    lookupKey key = foldr mergeCompat Nothing
      [ listToMaybe (mapMaybe (\a -> lookupPath ["api", a, key] bcd >>= compatOf) (ancestors o))
      | o <- owners ]
    member m = m { memBcd = listToMaybe (mapMaybe lookupKey (keys m)) }
    -- Candidate keys, most specific first.  BCD conventions: constructors are
    -- @api.X.X@, static and namespace members are @name_static@, and @onfoo@
    -- handlers are often only recorded as @foo_event@.
    keys m = case memKind m of
      MConstructor -> [defName d]
      k | k `elem` [MAttribute, MOperation, MConst], Just n <- memName m ->
            [ n <> "_static" | memSpecial m == "static" || defKind d == KNamespace ]
            <> [n]
            <> [ T.drop 2 n <> "_event" | k == MAttribute, "on" `T.isPrefixOf` n, isHandler m ]
      k | k `elem` [MIterable, MAsyncIterable] -> ["@@iterator", "@@asyncIterator"]
      _ -> []
    isHandler m = case memType m of
      Just Named { tyName } -> "EventHandler" `T.isSuffixOf` tyName
      _ -> False
