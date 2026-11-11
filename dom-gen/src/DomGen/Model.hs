-- | Web IDL model, decoded from the webidl2 AST JSON that webref publishes in
-- @ed/idlparsed/<spec>.json@, plus the event / element / BCD data that
-- "DomGen.Load" attaches.
module DomGen.Model
  ( -- * Assembled model
    Dom (..)
  , DefKind (..)
  , Definition (..)
  , Member (..)
  , MemberKind (..)
  , Argument (..)
  , IdlType (..)
  , ExtAttr (..)
  , Value (..)
  , Compat (..)
  , EnumDef (..)
  , Typedef (..)
  , CallbackDef (..)
  , EventDef (..)
  , EventTarget (..)
  , ElementDef (..)
    -- * Raw webref files
  , IdlParsed (..)
  , RawDef (..)
    -- * Helpers
  , hasExtAttr
  , extAttrRhs
  , engineCount
  ) where

import Control.Monad (mfilter)
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------------------------
-- Assembled model (built by DomGen.Load)

data Dom = Dom
  { domVersions :: A.Value
  , domDefinitions :: Map Text Definition   -- ^ interfaces, mixins, namespaces, dictionaries, callback interfaces
  , domEnums :: Map Text EnumDef
  , domTypedefs :: Map Text Typedef
  , domCallbacks :: Map Text CallbackDef
  , domEvents :: [EventDef]
  , domElements :: [ElementDef]
  }

data DefKind = KInterface | KMixin | KNamespace | KDictionary | KCallbackInterface
  deriving (Eq, Ord, Show)

data Definition = Definition
  { defName :: Text
  , defKind :: DefKind
  , defInheritance :: Maybe Text
  , defExtAttrs :: [ExtAttr]
  , defMembers :: [Member]
  , defSpec :: Text
  , defHref :: Maybe Text
  , defIncludes :: [Text]       -- ^ mixins this interface includes
  , defIncludedBy :: [Text]     -- ^ for mixins: interfaces including it
  , defBcd :: Maybe Compat
  }

data MemberKind
  = MAttribute | MOperation | MConstructor | MConst | MField
  | MIterable | MAsyncIterable | MMaplike | MSetlike
  deriving (Eq, Ord, Show)

-- | One flat record for all member kinds; fields that do not apply to a
-- kind keep their defaults.
data Member = Member
  { memKind :: MemberKind
  , memName :: Maybe Text
  , memType :: Maybe IdlType          -- ^ attribute/const/field type, operation return type
  , memArguments :: [Argument]
  , memReadonly :: Bool
  , memSpecial :: Text                -- ^ "" | static | getter | setter | deleter | stringifier | inherit
  , memRequired :: Bool
  , memDefault :: Maybe Value
  , memValue :: Maybe Value           -- ^ const value
  , memTypes :: [IdlType]             -- ^ iterable/maplike/setlike type arguments
  , memExtAttrs :: [ExtAttr]
  , memHref :: Maybe Text
  , memSpec :: Text                   -- ^ filled in by DomGen.Load
  , memBcd :: Maybe Compat            -- ^ filled in by DomGen.Load
  }

data Argument = Argument
  { argName :: Text
  , argType :: IdlType
  , argOptional :: Bool
  , argVariadic :: Bool
  , argDefault :: Maybe Value
  , argExtAttrs :: [ExtAttr]
  }

data IdlType
  = Named { tyName :: Text, tyNullable :: Bool, tyExtAttrs :: [ExtAttr] }
  | Generic { tyGeneric :: Text, tyArgs :: [IdlType], tyNullable :: Bool, tyExtAttrs :: [ExtAttr] }
  | Union { tyMembers :: [IdlType], tyNullable :: Bool, tyExtAttrs :: [ExtAttr] }

data ExtAttr = ExtAttr
  { eaName :: Text
  , eaRhs :: [Text]          -- ^ identifier(s) or literal; empty when absent
  , eaArguments :: [Argument]
  }

data Value
  = VString Text
  | VNumber Text
  | VBool Bool
  | VNull
  | VEmptySequence
  | VEmptyDictionary
  deriving (Eq, Show)

data Compat = Compat
  { cEngines :: [Text]       -- ^ subset of chromium, gecko, webkit with stable unprefixed support
  , cSince :: [(Text, Text)] -- ^ those engines' version_added (of the browser BCD tracks for it: Chrome, Firefox, Safari); "" if unknown
  , cPartial :: [Text]       -- ^ those of them whose support is only a partial_implementation
  , cDeprecated :: Bool
  , cExperimental :: Bool
  , cNonStandard :: Bool
  , cMdn :: Maybe Text
  } deriving (Show)

data EnumDef = EnumDef { enumName :: Text, enumValues :: [Text], enumSpec :: Text }

data Typedef = Typedef { typedefName :: Text, typedefType :: IdlType, typedefSpec :: Text }

data CallbackDef = CallbackDef
  { cbName :: Text
  , cbReturns :: IdlType
  , cbArguments :: [Argument]
  , cbExtAttrs :: [ExtAttr]
  , cbSpec :: Text
  }

data EventTarget = EventTarget
  { etTarget :: Text
  , etBubbles :: Maybe Bool
  }

data EventDef = EventDef
  { evType :: Text
  , evInterface :: Maybe Text
  , evTargets :: [EventTarget]
  , evCancelable :: Maybe Bool
  , evHref :: Maybe Text
  }

data ElementDef = ElementDef { elName :: Text, elInterface :: Maybe Text }

------------------------------------------------------------------------------
-- Raw webref data

-- | The parts of an @ed/idlparsed/<spec>.json@ file we use.
data IdlParsed = IdlParsed
  { ipNames :: Map Text RawDef          -- ^ @idlNames@: one main definition per name
  , ipExtended :: Map Text [RawDef]     -- ^ @idlExtendedNames@: partials and includes statements
  }

-- | A top level webidl2 definition.
data RawDef
  = RawDefinition Definition Bool       -- ^ definition and whether it is @partial@
  | RawEnum EnumDef
  | RawTypedef Typedef
  | RawCallback CallbackDef
  | RawIncludes Text Text               -- ^ target, mixin

instance A.FromJSON IdlParsed where
  parseJSON = A.withObject "idlparsed file" $ \o -> do
    p <- o .: "idlparsed"
    IdlParsed <$> p .:? "idlNames" .!= mempty <*> p .:? "idlExtendedNames" .!= mempty

instance A.FromJSON RawDef where
  parseJSON = A.withObject "definition" $ \o -> do
    ty <- o .: "type"
    let defn kind = do
          name <- o .: "name"
          inh <- o .:? "inheritance"
          eas <- o .:? "extAttrs" .!= []
          ms <- o .:? "members" .!= []
          partial <- o .:? "partial" .!= False
          href <- o .:? "href"
          pure $ RawDefinition
            (Definition name kind inh eas ms "" href [] [] Nothing) partial
    case (ty :: Text) of
      "interface" -> defn KInterface
      "interface mixin" -> defn KMixin
      "namespace" -> defn KNamespace
      "dictionary" -> defn KDictionary
      "callback interface" -> defn KCallbackInterface
      "enum" -> do
        vs <- o .: "values" >>= mapM (A.withObject "enum value" (.: "value"))
        n <- o .: "name"
        pure $ RawEnum (EnumDef n vs "")
      "typedef" -> RawTypedef <$> (Typedef <$> o .: "name" <*> o .: "idlType" <*> pure "")
      "callback" -> RawCallback <$>
        (CallbackDef <$> o .: "name" <*> o .: "idlType" <*> o .: "arguments"
           <*> o .:? "extAttrs" .!= [] <*> pure "")
      "includes" -> RawIncludes <$> o .: "target" <*> o .: "includes"
      _ -> fail ("unknown definition type " <> T.unpack ty)

instance A.FromJSON Member where
  parseJSON = A.withObject "member" $ \o -> do
    ty <- o .: "type"
    name <- o .:? "name"
    let base kind = Member kind (mfilter (not . T.null) name) Nothing <$> o .:? "arguments" .!= []
          <*> o .:? "readonly" .!= False <*> o .:? "special" .!= "" <*> o .:? "required" .!= False
          <*> o .:? "default" <*> o .:? "value" <*> pure [] <*> o .:? "extAttrs" .!= []
          <*> o .:? "href" <*> pure "" <*> pure Nothing
        typed kind = do
          m <- base kind
          t <- o .:? "idlType"   -- absent for a bare `stringifier;`
          pure m { memType = t }
        decl kind = do
          m <- base kind
          ts <- o .: "idlType"
          pure m { memTypes = ts }
    case (ty :: Text) of
      "attribute" -> typed MAttribute
      "operation" -> typed MOperation
      "const" -> typed MConst
      "field" -> typed MField
      "constructor" -> base MConstructor
      "iterable" -> do
        async <- o .:? "async" .!= False
        decl (if async then MAsyncIterable else MIterable)
      "async_iterable" -> decl MAsyncIterable
      "maplike" -> decl MMaplike
      "setlike" -> decl MSetlike
      _ -> fail ("unknown member type " <> T.unpack ty)

instance A.FromJSON Argument where
  parseJSON = A.withObject "argument" $ \o ->
    Argument <$> o .: "name" <*> o .: "idlType" <*> o .:? "optional" .!= False
      <*> o .:? "variadic" .!= False <*> o .:? "default" <*> o .:? "extAttrs" .!= []

instance A.FromJSON IdlType where
  parseJSON = A.withObject "idlType" $ \o -> do
    nullable <- o .:? "nullable" .!= False
    eas <- o .:? "extAttrs" .!= []
    generic <- o .:? "generic" .!= ""
    union <- o .:? "union" .!= False
    if | union -> (\ms -> Union ms nullable eas) <$> o .: "idlType"
       | not (T.null generic) -> (\as -> Generic generic as nullable eas) <$> o .: "idlType"
       | otherwise -> (\n -> Named n nullable eas) <$> o .: "idlType"

instance A.FromJSON ExtAttr where
  parseJSON = A.withObject "extAttr" $ \o -> do
    rhs <- o .:? "rhs" >>= \case
      Nothing -> pure []
      Just r -> flip (A.withObject "rhs") r $ \ro -> do
        rty <- ro .: "type"
        ro .: "value" >>= \case
          A.String s -> pure [s]
          v@(A.Array _) -> A.parseJSON v >>= mapM (A.withObject "rhs item" (.: "value"))
          A.Null | rty == ("*" :: Text) -> pure ["*"]   -- [Exposed=*]
          v -> A.typeMismatch "extended attribute rhs" v
    ExtAttr <$> o .: "name" <*> pure rhs <*> o .:? "arguments" .!= []

instance A.FromJSON Value where
  parseJSON = A.withObject "value" $ \o -> o .: "type" >>= \case
    "string" -> VString <$> o .: "value"
    "number" -> VNumber <$> o .: "value"
    "boolean" -> VBool <$> o .: "value"
    "null" -> pure VNull
    "Infinity" -> do
      neg <- o .:? "negative" .!= False
      pure (VNumber (if neg then "-Infinity" else "Infinity"))
    "NaN" -> pure (VNumber "NaN")
    "sequence" -> pure VEmptySequence
    "dictionary" -> pure VEmptyDictionary
    t -> fail ("unknown value type " <> T.unpack t)

instance A.FromJSON EventTarget where
  parseJSON = A.withObject "event target" $ \o ->
    EventTarget <$> o .: "target" <*> o .:? "bubbles"

instance A.FromJSON EventDef where
  parseJSON = A.withObject "event" $ \o ->
    EventDef <$> o .: "type" <*> o .:? "interface" <*> o .:? "targets" .!= []
      <*> o .:? "cancelable" <*> o .:? "href"

instance A.FromJSON ElementDef where
  parseJSON = A.withObject "element" $ \o ->
    ElementDef <$> o .: "name" <*> o .:? "interface"

------------------------------------------------------------------------------

hasExtAttr :: Text -> [ExtAttr] -> Bool
hasExtAttr n = any ((== n) . eaName)

extAttrRhs :: Text -> [ExtAttr] -> [Text]
extAttrRhs n eas = concat [eaRhs ea | ea <- eas, eaName ea == n]

engineCount :: Maybe Compat -> Int
engineCount = maybe 0 (length . cEngines)
