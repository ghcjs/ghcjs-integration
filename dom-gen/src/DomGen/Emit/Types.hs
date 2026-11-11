-- | The generated part of @JSDOM.Types@: newtypes, Is-classes and instances
-- for every typed definition, union and callback.
--
-- @JSDOM/Types.hs@ is a hand written file with two generated regions,
-- delimited by @-- AUTO GENERATION STARTS HERE@ / @-- AUTO GENERATION ENDS
-- HERE@ markers: the export list section and the tail of the file.
module DomGen.Emit.Types
  ( Flavour (..)
  , typesValueNames
  , spliceTypes
  ) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import DomGen.Env
import DomGen.Model

data TypeKind = TInterface | TMixin | TDictionary | TBuiltin
  deriving (Eq)

-- | Which package's Types module: jsaddle-dom (@JSDOM.Types@) or
-- ghcjs-dom-javascript (@GHCJS.DOM.Types@).
data Flavour = JSaddle | JSFFI
  deriving (Eq)

-- | Types to emit, in name order.
typeEntries :: Env -> [(Text, TypeKind)]
typeEntries env = sort' $
  [ (n, k) | n <- S.toList (envTyped env), Just d <- [M.lookup n (envDefs env)]
           , Just k <- [kindOf (defKind d)] ]
  <> [ (n, TBuiltin) | n <- extraBuiltins ]
  where
    kindOf = \case
      KInterface -> Just TInterface
      KMixin -> Just TMixin
      KDictionary -> Just TDictionary
      KCallbackInterface -> Just TDictionary   -- opaque (EventListener); gType Object
      _ -> Nothing
    sort' = M.toList . M.fromList

-- | Value names exported by the generated part of JSDOM.Types (generated
-- modules hide these when they define a binding of the same name).
typesValueNames :: Env -> S.Set Text
typesValueNames env = S.fromList $ concat
  [ ["un" <> n, "no" <> n, "gType" <> n] <> [ "to" <> n | hasClass env n ]
  | (n, _) <- typeEntries env ]
  <> concat [ ["un" <> n, "to" <> n] | n <- M.keys (envUnions env) ]
  <> [ "no" <> n | n <- S.toList (envCallbacks env) ]
  <> concat [ [p <> n | p <- ["to", "no", "un", "gType"]] | n <- headerTypeNames ]
  <> headerValues

-- | Types defined in the hand written part of JSDOM.Types.
headerTypeNames :: [Text]
headerTypeNames = M.keys headerTypes <>
  ["GObject", "RawTypedArray", "Date", "Array", "ObjectArray", "Dictionary", "Record", "GlobalThis"]

-- | Values exported by the hand written part of JSDOM.Types.
headerValues :: [Text]
headerValues =
  [ "askJSM", "runJSM", "liftJSM", "askDOM", "runDOM", "liftDOM", "toJSVal", "fromJSVal"
  , "fromJSValUnchecked", "pToJSVal", "pFromJSVal", "toJSString", "fromJSString"
  , "toMaybeJSString", "fromMaybeJSString", "noJSString", "fromJSArray", "fromJSArrayUnchecked"
  , "nullableToMaybe", "maybeToNullable", "maybeNullOrUndefined", "maybeNullOrUndefined'"
  , "isA", "objectToString", "castTo", "unsafeCastTo", "uncheckedCastTo", "strictEqual"
  , "readPromise", "withCallback", "typeGType", "toGObject", "noGObject", "gTypeGObject"
  , "rejectionReason", "noPromiseRejected", "integralToDoubleToJSVal"
  , "integralFromDoubleFromJSVal", "integralFromDoubleFromJSValUnchecked"
  -- re-exported from Language.Javascript.JSaddle.Int53
  , "int53", "toInt53", "clampInt53", "fromInt53", "minInt53", "maxInt53"
  , "word53", "toWord53", "clampWord53", "fromWord53", "maxWord53"
  ]

------------------------------------------------------------------------------

exportLines :: Env -> [Text]
exportLines env =
  [ "  , " <> n <> "(" <> n <> "), un" <> n <> ", " <> className n <> ", to" <> n
  | n <- M.keys (envUnions env) ]
  <> [ "  , " <> n <> "(..), no" <> n | n <- S.toAscList (envCallbacks env) ]
  <> [ "  , " <> T.intercalate ", " (
         [n <> "(" <> n <> ")", "un" <> n] <> (if hasClass env n then [className n, "to" <> n] else [])
         <> ["no" <> n, "gType" <> n])
     | (n, _) <- typeEntries env ]

newtypeBlock :: Flavour -> Text -> [Text]
newtypeBlock JSFFI n =
  [ "newtype " <> n <> " = " <> n <> " { un" <> n <> " :: JSVal }"
  , ""
  , "instance Eq " <> n <> " where"
  , "  (" <> n <> " a) == (" <> n <> " b) = js_eq a b"
  , ""
  , "instance PToJSVal " <> n <> " where"
  , "  pToJSVal = un" <> n
  , "  {-# INLINE pToJSVal #-}"
  , ""
  , "instance PFromJSVal " <> n <> " where"
  , "  pFromJSVal = " <> n
  , "  {-# INLINE pFromJSVal #-}"
  , ""
  , "instance ToJSVal " <> n <> " where"
  , "  toJSVal = return . un" <> n
  , "  {-# INLINE toJSVal #-}"
  , ""
  , "instance FromJSVal " <> n <> " where"
  , "  fromJSVal = return . fmap " <> n <> " . maybeJSNullOrUndefined"
  , "  {-# INLINE fromJSVal #-}"
  , "  fromJSValUnchecked = return . " <> n
  , "  {-# INLINE fromJSValUnchecked #-}"
  , ""
  ]
newtypeBlock JSaddle n =
  [ "newtype " <> n <> " = " <> n <> " { un" <> n <> " :: JSVal }"
  , ""
  , "instance PToJSVal " <> n <> " where"
  , "  pToJSVal = un" <> n
  , "  {-# INLINE pToJSVal #-}"
  , ""
  , "instance PFromJSVal " <> n <> " where"
  , "  pFromJSVal = " <> n
  , "  {-# INLINE pFromJSVal #-}"
  , ""
  , "instance ToJSVal " <> n <> " where"
  , "  toJSVal = return . un" <> n
  , "  {-# INLINE toJSVal #-}"
  , ""
  , "instance FromJSVal " <> n <> " where"
  , "  fromJSVal v = fmap " <> n <> " <$> maybeNullOrUndefined v"
  , "  {-# INLINE fromJSVal #-}"
  , "  fromJSValUnchecked = return . " <> n
  , "  {-# INLINE fromJSValUnchecked #-}"
  , ""
  , "instance MakeObject " <> n <> " where"
  , "  makeObject = makeObject . un" <> n
  , ""
  ]

typeBlock :: Flavour -> Env -> (Text, TypeKind) -> [Text]
typeBlock fl env (n, kind) =
  doc <> newtypeBlock fl n <> classDecl <> instances <>
  [ "instance IsGObject " <> n <> " where"
  , "  typeGType _ = gType" <> n
  , "  {-# INLINE typeGType #-}"
  , ""
  , "no" <> n <> " :: Maybe " <> n
  , "no" <> n <> " = Nothing"
  , "{-# INLINE no" <> n <> " #-}"
  , ""
  ] <> gType <> [""]
  where
    gType = case fl of
      JSaddle ->
        [ "gType" <> n <> " :: JSM GType"
        , "gType" <> n <> " = GType . Object <$> jsg \"" <> global <> "\"" ]
      JSFFI ->
        [ "foreign import javascript unsafe \"(() => globalThis[\\\"" <> global <> "\\\"])\" gType" <> n <> " :: GType" ]
    root = case fl of JSaddle -> "JSDOM."; JSFFI -> "GHCJS.DOM."
    def = M.lookup n (envDefs env)
    global = case (kind, def) of
      (TInterface, Just d) | not (hasExtAttr "LegacyNoInterfaceObject" (defExtAttrs d)) -> n
      (TBuiltin, _) -> n
      _ -> "Object"
    doc = case (kind, def) of
      (TDictionary, Just d) -> ["-- | Dictionary " <> n <> " (" <> defSpec d <> ")"]
      (_, Just d) -> ["-- | Functions for this interface are in \"" <> root <> n <> "\" (" <> defSpec d <> ")."]
      _ -> []
    classDecl
      | hasClass env n =
          [ "class (" <> T.intercalate ", " (map (<> " o") (superClasses env n) <> ["IsGObject o"]) <> ") => " <> className n <> " o"
          , "to" <> n <> " :: " <> className n <> " o => o -> " <> n
          , "to" <> n <> " = " <> n <> " . coerce"
          , "" ]
      | otherwise = []
    instances =
      [ "instance " <> c <> " " <> n | c <- instanceClasses env n ]
      <> [ "instance IsArrayBufferView " <> n | kind == TBuiltin, builtinParent n == Just "ArrayBufferView" ]
      <> [""]

unionBlock :: Flavour -> Env -> (Text, [IdlType]) -> [Text]
unionBlock fl env (n, members) =
  newtypeBlock fl n <> conversion <>
  [ ""
  , "instance " <> className n <> " " <> n
  ] <> [ "instance " <> className n <> " " <> t | t <- instanceTypes ] <> [""]
  where
    conversion = case fl of
      JSaddle ->
        [ "class (ToJSVal o) => " <> className n <> " o"
        , ""
        , "to" <> n <> " :: (MonadJSM m, " <> className n <> " o) => o -> m " <> n
        , "to" <> n <> " o = liftJSM (" <> n <> " <$> toJSVal o)" ]
      JSFFI ->
        [ "class (PToJSVal o, ToJSVal o) => " <> className n <> " o"
        , ""
        , "to" <> n <> " :: " <> className n <> " o => o -> " <> n
        , "to" <> n <> " = " <> n <> " . pToJSVal" ]
    instanceTypes = filter (/= n) . nub . sort $ concatMap (memberTypes . resolve env) members
    typedSet = envTyped env
    -- a type and everything that can be used where it is expected
    withSubtypes t = t : concat
      [ withSubtypes c | c <- M.findWithDefault [] t (envChildren env) ]
      <> concat [ withSubtypes i | Just d <- [M.lookup t (envDefs env)], defKind d == KMixin
                                 , i <- defIncludedBy d, S.member i typedSet ]
      <> [ b | t == "ArrayBufferView", b <- extraBuiltins <> builtinTypedArrays, builtinParent b == Just t ]
    builtinTypedArrays = [ b | b <- M.keys headerTypes, builtinParent b == Just "ArrayBufferView" ]
    memberTypes = \case
      HString -> ["T.Text", "JSString", "String"]
      HBool -> ["Bool"]
      HNum t -> [t]
      HObject -> ["GObject"]
      HCallback c -> [c]
      HRef r _
        | M.member r (envUnions env) -> []  -- nested unions are flattened
        | otherwise -> withSubtypes r
      _ -> []

callbackBlock :: Flavour -> Text -> [Text]
callbackBlock fl n =
  [ "newtype " <> n <> " = " <> n <> " (Callback (JSVal -> IO ()))"
  , "no" <> n <> " :: Maybe " <> n
  , "no" <> n <> " = Nothing"
  , "{-# INLINE no" <> n <> " #-}"
  ] <> instances <> [""]
  where
    instances = case fl of
      JSaddle -> [ "instance ToJSVal " <> n <> " where toJSVal (" <> n <> " (Callback r)) = toJSVal r" ]
      JSFFI ->
        [ "instance PToJSVal " <> n <> " where pToJSVal (" <> n <> " r) = jsval r"
        , "instance ToJSVal " <> n <> " where toJSVal = return . pToJSVal" ]

generatedTail :: Flavour -> Env -> [Text]
generatedTail fl env =
  [ "-- The remainder of this file is generated from webref IDL by dom-gen. Do not edit." ]
  <> concatMap (unionBlock fl env) (M.toList (envUnions env))
  <> concatMap (callbackBlock fl) (S.toAscList (envCallbacks env))
  <> concatMap (typeBlock fl env) (typeEntries env)

-- | Replace the two generated regions of an existing JSDOM/Types.hs (or
-- GHCJS/DOM/Types.hs for 'JSFFI').
spliceTypes :: Flavour -> Env -> Text -> Either String Text
spliceTypes fl env src =
  case T.splitOn startMarker src of
    [header, exportsAndMiddle, _oldTail] ->
      case T.splitOn endMarker exportsAndMiddle of
        [_oldExports, middle] -> Right $ T.concat
          [ header, startMarker, "\n", T.unlines (exportLines env)
          , endMarker, middle, startMarker, "\n", T.unlines (generatedTail fl env) ]
        _ -> Left "JSDOM/Types.hs: expected one AUTO GENERATION ENDS HERE marker"
    _ -> Left "JSDOM/Types.hs: expected two AUTO GENERATION STARTS HERE markers"
  where
    startMarker = "-- AUTO GENERATION STARTS HERE"
    endMarker = "-- AUTO GENERATION ENDS HERE"
