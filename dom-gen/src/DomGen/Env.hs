-- | Inclusion policy, type resolution and the derived facts (closure of
-- referenced types, unions, class hierarchy, event typing) that all the
-- emitters share.
module DomGen.Env
  ( Env (..)
  , HTy (..)
  , buildEnv
    -- * Policy
  , memberIncluded
  , defMembersIncluded
    -- * Types
  , resolve
  , hasClass
  , className
  , superClasses
  , instanceClasses
  , isEventHandlerType
  , unionHsName
    -- * Hierarchy
  , ancestors
  , descendants
  , isEventTarget
    -- * Events
  , EventBinding (..)
  , eventBindings
    -- * Builtins
  , headerTypes
  , extraBuiltins
  , builtinParent
  ) where

import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import DomGen.Baseline
import DomGen.Model
import DomGen.Names

-- | Haskell view of an IDL type.
data HTy
  = HUnit
  | HBool
  | HNum Text               -- ^ Int, Word, Int64, Word64, Double, Float
  | HString                 -- ^ any IDL string type (ToJSString/FromJSString polymorphic)
  | HAny                    -- ^ any, bigint, symbol: plain JSVal
  | HObject                 -- ^ object
  | HRef Text (Maybe Text)  -- ^ newtype and, when it has one, its Is-class
  | HCallback Text
  | HEnum Text
  | HSeq HTy
  | HPromise HTy
  | HMaybe HTy
  | HRecord
  | HUnsupported Text       -- ^ members using these are skipped
  deriving (Eq, Ord, Show)

data Env = Env
  { envDom :: Dom
  , envBaseline :: Baseline
  , envDefs :: M.Map Text Definition
  , envChildren :: M.Map Text [Text]          -- ^ typed direct descendants (interfaces and dictionaries)
  , envTyped :: S.Set Text                    -- ^ definitions that get a Haskell type
  , envEnums :: S.Set Text
  , envCallbacks :: S.Set Text                -- ^ callbacks and callback interfaces used
  , envUnions :: M.Map Text [IdlType]         -- ^ union newtype -> member types (flattened)
  , envUnionNames :: M.Map Text Text          -- ^ structural union key -> union newtype
  , envAliases :: M.Map Text Text             -- ^ LegacyWindowAlias names and WindowProxy
  }

------------------------------------------------------------------------------
-- Builtins

-- | Types defined by hand in the non generated part of JSDOM.Types, with the
-- name of their Is-class.
headerTypes :: M.Map Text Text
headerTypes = M.fromList
  [ (t, "Is" <> t)
  | t <- [ "ArrayBuffer", "ArrayBufferView", "Float32Array", "Float64Array", "Uint8Array"
         , "Uint8ClampedArray", "Uint16Array", "Uint32Array", "Int8Array", "Int16Array"
         , "Int32Array", "Function" ] ]

-- | Web IDL buffer types not in the JSDOM.Types header; generated like
-- interfaces.
extraBuiltins :: [Text]
extraBuiltins = ["DataView", "SharedArrayBuffer", "Float16Array", "BigInt64Array", "BigUint64Array"]

-- | Callback interfaces that are plain newtypes rather than callbacks:
-- EventListener values are made by JSDOM.EventTargetClosures (which wraps
-- Haskell handlers), not by a generated @newEventListener@.
opaqueCallbackInterfaces :: [Text]
opaqueCallbackInterfaces = ["EventListener"]

-- | ArrayBufferView covers every typed array and DataView.
builtinParent :: Text -> Maybe Text
builtinParent t
  | t `elem` ["Float32Array", "Float64Array", "Uint8Array", "Uint8ClampedArray", "Uint16Array"
             , "Uint32Array", "Int8Array", "Int16Array", "Int32Array", "DataView"
             , "Float16Array", "BigInt64Array", "BigUint64Array"] = Just "ArrayBufferView"
  | otherwise = Nothing

primitive :: Text -> Maybe HTy
primitive = \case
  "boolean" -> Just HBool
  "byte" -> Just (HNum "Int")
  "short" -> Just (HNum "Int")
  "long" -> Just (HNum "Int")
  "octet" -> Just (HNum "Word")
  "unsigned short" -> Just (HNum "Word")
  "unsigned long" -> Just (HNum "Word")
  -- JS Numbers: exact only up to 2^53 (Language.Javascript.JSaddle.Int53;
  -- deliberately no Num instance)
  "long long" -> Just (HRef "Int53" Nothing)
  "unsigned long long" -> Just (HRef "Word53" Nothing)
  "float" -> Just (HNum "Float")
  "unrestricted float" -> Just (HNum "Float")
  "double" -> Just (HNum "Double")
  "unrestricted double" -> Just (HNum "Double")
  "DOMString" -> Just HString
  "USVString" -> Just HString
  "ByteString" -> Just HString
  "CSSOMString" -> Just HString
  "any" -> Just HAny
  "bigint" -> Just HAny
  "symbol" -> Just HAny
  "object" -> Just HObject
  "undefined" -> Just HUnit
  "void" -> Just HUnit
  _ -> Nothing

isEventHandlerType :: IdlType -> Bool
isEventHandlerType = \case
  Named { tyName } -> tyName `elem` ["EventHandler", "OnErrorEventHandler", "OnBeforeUnloadEventHandler"]
  _ -> False

------------------------------------------------------------------------------
-- Policy

defEngines :: Definition -> Int
defEngines = engineCount . defBcd

-- | Interface and namespace level selection: shipped in at least two
-- engines, or in the old bindings and shipped somewhere (or unknown to BCD).
defSelected :: Baseline -> Definition -> Bool
defSelected bl d = case defKind d of
  k | k `elem` [KInterface, KNamespace] ->
      defEngines d >= 2 || (M.member (defName d) (blMembers bl) && (defEngines d >= 1 || isNothing (defBcd d)))
  _ -> False

-- | The names under which the old bindings may have exposed members of @d@.
oldOwners :: Definition -> [Text]
oldOwners d = defName d : defIncludedBy d

oldMemberKey :: Member -> Maybe Text
oldMemberKey m = case memKind m of
  MConstructor -> Just "constructor"
  _ -> memName m

memberIncluded :: Env -> Definition -> Member -> Bool
memberIncluded env d m = case memKind m of
  MIterable -> False
  MAsyncIterable -> False
  MMaplike -> False
  MSetlike -> False
  MField -> True
  MConst -> True
  MConstructor
    | hasExtAttr "HTMLConstructor" (memExtAttrs m) -> False
  MOperation
    | isNothing (memName m) -> False         -- unnamed getter/setter/deleter/stringifier
  _ ->
    let e = engineCount (memBcd m)
        old = maybe False (\k -> any (\o -> oldHasMember (envBaseline env) o k) (oldOwners d)) (oldMemberKey m)
    in supported (memTypesUsed m) && (e >= 2 || (old && (e >= 1 || isNothing (memBcd m))))
  where
    supported = all (not . unsupported . resolve env)
    unsupported = \case
      HUnsupported _ -> True
      HSeq t -> unsupported t
      HPromise t -> unsupported t
      HMaybe t -> unsupported t
      _ -> False

memTypesUsed :: Member -> [IdlType]
memTypesUsed m = [ t | Just t <- [memType m], not (isEventHandlerType t) ] <> map argType (memArguments m)

defMembersIncluded :: Env -> Definition -> [Member]
defMembersIncluded env d = filter (memberIncluded env d) (defMembers d)

------------------------------------------------------------------------------
-- Type resolution

-- | Name of the Haskell newtype for a union.  The names are computed once in
-- 'buildEnv'; this is only a lookup (resolving members here would recurse
-- forever for unions containing e.g. @sequence<(A or B)>@).
unionHsName :: Env -> IdlType -> Text
unionHsName env t = M.findWithDefault (anonUnionName t) (unionKey (envDom env) t) (envUnionNames env)

-- | Order independent, purely structural identity of a union (string
-- types are equivalent; nullability is ignored).
unionKey :: Dom -> IdlType -> Text
unionKey dom t = "(" <> T.intercalate "|" (sort (nub (map (typeKey dom) (flattenUnionDom dom t)))) <> ")"

typeKey :: Dom -> IdlType -> Text
typeKey dom = \case
  Named { tyName }
    | tyName `elem` ["DOMString", "USVString", "ByteString", "CSSOMString"] -> "string"
    | otherwise -> tyName
  Generic { tyGeneric, tyArgs } -> tyGeneric <> "<" <> T.intercalate "," (map (typeKey dom) tyArgs) <> ">"
  u@Union {} -> unionKey dom u

anonUnionName :: IdlType -> Text
anonUnionName = \case
  Union { tyMembers } -> T.intercalate "Or" (map part tyMembers)
  t -> part t
  where
    part = \case
      Named { tyName } -> case tyName of
        "DOMString" -> "String"
        "USVString" -> "String"
        "ByteString" -> "String"
        "CSSOMString" -> "String"
        "unrestricted double" -> "Double"
        "double" -> "Double"
        "unsigned long" -> "Word"
        "long" -> "Int"
        "boolean" -> "Bool"
        "undefined" -> "Undefined"
        n -> conIdent n
      Generic { tyGeneric, tyArgs } -> conIdent tyGeneric <> T.concat (map part tyArgs)
      u@Union {} -> anonUnionName u

-- | Members of a union with nested unions (including union typedefs)
-- expanded.
flattenUnion :: Env -> IdlType -> [IdlType]
flattenUnion = flattenUnionDom . envDom

flattenUnionDom :: Dom -> IdlType -> [IdlType]
flattenUnionDom dom = \case
  Union { tyMembers } -> concatMap (flattenUnionDom dom) tyMembers
  t@Named { tyName }
    | Just td <- M.lookup tyName (domTypedefs dom)
    , Union {} <- typedefType td
    , not (M.member tyName headerTypes) -> flattenUnionDom dom (typedefType td)
    | otherwise -> [t]
  t -> [t]

resolve :: Env -> IdlType -> HTy
resolve env t
  | tyNullable t = case resolve env t { tyNullable = False } of
      HAny -> HAny
      HUnit -> HUnit
      h -> HMaybe h
resolve env t = case t of
  Union {} -> unionRef (unionHsName env t)
  Generic { tyGeneric, tyArgs } -> case (tyGeneric, tyArgs) of
    ("sequence", [a]) -> HSeq (resolve env a)
    ("FrozenArray", [a]) -> HSeq (resolve env a)
    ("ObservableArray", [a]) -> HSeq (resolve env a)
    ("Promise", [a]) -> HPromise (resolve env a)
    ("record", _) -> HRecord
    _ -> HUnsupported tyGeneric
  Named { tyName = n0 } ->
    let n = M.findWithDefault n0 n0 (envAliases env)
        dom = envDom env
    in if | Just p <- primitive n -> p
          | Just cls <- M.lookup n headerTypes -> HRef n (Just cls)
          | n `elem` extraBuiltins -> HRef n Nothing
          | Just d <- M.lookup n (envDefs env) -> case defKind d of
              KCallbackInterface
                | n `elem` opaqueCallbackInterfaces -> HRef n Nothing
                | otherwise -> HCallback n
              _ -> HRef n (if hasClass env n then Just (className n) else Nothing)
          | M.member n (domEnums dom) -> HEnum n
          | Just cb <- M.lookup n (domCallbacks dom) ->
              if "EventHandlerNonNull" `T.isSuffixOf` cbName cb then HUnsupported n else HCallback n
          | Just td <- M.lookup n (domTypedefs dom) -> case typedefType td of
              Union {} -> unionRef n
              ty -> resolve env ty { tyNullable = tyNullable ty }
          | otherwise -> HUnsupported n
  where
    unionRef n = HRef n (Just (className n))

className :: Text -> Text
className n = "Is" <> n

hasClass :: Env -> Text -> Bool
hasClass env n
  | M.member n (envUnions env) = True
  | Just d <- M.lookup n (envDefs env) = defKind d == KMixin || not (null (M.findWithDefault [] n (envChildren env)))
  | otherwise = False

------------------------------------------------------------------------------
-- Hierarchy

ancestors :: Env -> Text -> [Text]
ancestors env n = case M.lookup n (envDefs env) >>= defInheritance of
  Just p -> p : ancestors env p
  Nothing -> []

-- | Typed descendants (transitively).
descendants :: Env -> Text -> [Text]
descendants env n = concat [ c : descendants env c | c <- M.findWithDefault [] n (envChildren env) ]

isEventTarget :: Env -> Text -> Bool
isEventTarget env n = n == "EventTarget" || "EventTarget" `elem` ancestors env n

-- | Direct superclasses of @n@'s Is-class: parent class and own mixins.
superClasses :: Env -> Text -> [Text]
superClasses env n = case M.lookup n (envDefs env) of
  Nothing -> []
  Just d ->
    [ className p | Just p <- [defInheritance d] ]
      <> [ className mx | mx <- defIncludes d, S.member mx (envTyped env) ]

-- | Every Is-class @n@ is an instance of (its own, ancestors', and all their
-- mixins').
instanceClasses :: Env -> Text -> [Text]
instanceClasses env n = nub $ concat
  [ [ className a | hasClass env a ] <> [ className mx | Just d <- [M.lookup a (envDefs env)], mx <- defIncludes d, S.member mx (envTyped env) ]
  | a <- n : ancestors env n ]

------------------------------------------------------------------------------
-- Construction: closure of referenced definitions

buildEnv :: Dom -> Baseline -> Env
buildEnv dom bl = env
  where
    defs = domDefinitions dom
    aliases = M.fromList $
      ("WindowProxy", "Window") :
      [ (a, defName d) | d <- M.elems defs, a <- extAttrRhs "LegacyWindowAlias" (defExtAttrs d) ]

    -- A preliminary environment for resolving types while computing the
    -- closure (classes are not known yet; they do not affect references).
    pre = Env dom bl defs mempty mempty mempty mempty mempty mempty aliases

    seeds = [ defName d | d <- M.elems defs, defSelected bl d ]

    (typed0, enums, cbs, unions) = go S.empty S.empty S.empty M.empty seeds

    -- Mixins none of whose members are included (e.g. GeometryUtils, only
    -- in Gecko, or CSS Regions' Region) would only add empty Is-classes to
    -- every including interface's hierarchy; leave them out.
    typed = S.filter (not . emptyMixin) typed0
    emptyMixin n = case M.lookup n defs of
      Just d | defKind d == KMixin ->
        null (filter (memberIncluded pre d) (defMembers d)) && null (eventBindings pre d)
      _ -> False

    go :: S.Set Text -> S.Set Text -> S.Set Text -> M.Map Text [IdlType] -> [Text]
       -> (S.Set Text, S.Set Text, S.Set Text, M.Map Text [IdlType])
    go ty en cb un [] = (ty, en, cb, un)
    go ty en cb un (n : rest)
      | S.member n ty || S.member n en || S.member n cb || M.member n un = go ty en cb un rest
      -- defined by hand in the JSDOM.Types header (or generated as builtins)
      | M.member n headerTypes || n `elem` extraBuiltins = go ty en cb un rest
      | Just d <- M.lookup n defs =
          let ty' = S.insert n ty
              deps = [ p | Just p <- [defInheritance d] ] <> defIncludes d
                <> concatMap (refsOf . Just) (concatMap memTypesAll (filter (memberIncluded pre d) (defMembers d)))
                <> eventIfaces d
          in case defKind d of
               KCallbackInterface
                 | n `elem` opaqueCallbackInterfaces -> go ty' en cb un rest
                 | otherwise -> go ty en (S.insert n cb) un (deps <> rest)
               _ -> go ty' en cb un (deps <> rest)
      | M.member n (domEnums dom) = go ty (S.insert n en) cb un rest
      -- event handler attributes are bound as EventName values instead
      | "EventHandlerNonNull" `T.isSuffixOf` n = go ty en cb un rest
      | Just c <- M.lookup n (domCallbacks dom) =
          go ty en (S.insert n cb) un (concatMap (refsOf . Just) (cbReturns c : map argType (cbArguments c)) <> rest)
      | Just td <- M.lookup n (domTypedefs dom) = case typedefType td of
          u@Union {} ->
            let ms = flattenUnion pre u
            in go ty en cb (M.insert n ms un) (concatMap (refsOf . Just) ms <> rest)
          other -> go ty en cb un (refsOf (Just other) <> rest)
      | otherwise = go ty en cb un rest

    -- Names referenced by a type; anonymous unions are registered under
    -- their generated name (as a pseudo reference).
    refsOf :: Maybe IdlType -> [Text]
    refsOf = \case
      Nothing -> []
      Just t -> case t of
        Named { tyName } -> [M.findWithDefault tyName tyName aliases]
        Generic { tyArgs } -> concatMap (refsOf . Just) tyArgs
        u@Union {} -> concatMap (refsOf . Just) (flattenUnion pre u)

    memTypesAll m = [ t | Just t <- [memType m] ] <> map argType (memArguments m)

    -- Event interfaces of handler attributes are referenced too.
    eventIfaces d = [ e | b <- eventBindings pre d, Just e <- [Just (ebInterface b)] ]

    anonUnions =
      M.fromList
        [ (anonUnionName u, flattenUnion pre u)
        | n <- S.toList typed
        , Just d <- [M.lookup n defs]
        , m <- filter (memberIncluded pre d) (defMembers d)
        , t <- memTypesAll m
        , u <- unionsIn t
        ]
      <> M.fromList
        [ (anonUnionName u, flattenUnion pre u)
        | n <- S.toList cbs, Just c <- [M.lookup n (domCallbacks dom)]
        , t <- cbReturns c : map argType (cbArguments c), u <- unionsIn t ]
    unionsIn = \case
      u@Union {} -> u : concatMap unionsIn (tyMembers u)
      Generic { tyArgs } -> concatMap unionsIn tyArgs
      _ -> []

    anonOnly = M.filterWithKey (\k _ -> not (M.member k unions) && not (M.member k defs) && not (M.member k headerTypes)) anonUnions
    allUnions = M.union unions anonOnly
    -- Typedef names win over generated names for the same set of members.
    unionNames = M.fromList $
      [ (unionKey dom (Union ms False []), n) | (n, ms) <- M.toList anonOnly ]
      <> [ (unionKey dom (Union ms False []), n) | (n, ms) <- M.toList unions ]

    children = M.fromListWith (<>)
      [ (p, [defName d]) | n <- S.toList typed, Just d <- [M.lookup n defs], Just p <- [defInheritance d] ]

    env = Env
      { envDom = dom
      , envBaseline = bl
      , envDefs = defs
      , envChildren = M.map sort children
      , envTyped = typed
      , envEnums = enums
      , envCallbacks = cbs
      , envUnions = allUnions
      , envUnionNames = unionNames
      , envAliases = aliases
      }

------------------------------------------------------------------------------
-- Events

data EventBinding = EventBinding
  { ebType :: Text          -- ^ JS event type
  , ebHsName :: Text
  , ebInterface :: Text     -- ^ event interface (MouseEvent, ...)
  , ebAsync :: Bool
  , ebHref :: Maybe Text
  }

-- | Event bindings for a definition: one per EventHandler attribute, plus
-- events @webref/events@ lists for this exact target that have no handler
-- attribute (e.g. focusin on Element, DOMContentLoaded on Document).
eventBindings :: Env -> Definition -> [EventBinding]
eventBindings env d = handlerEvents <> extraEvents
  where
    bl = envBaseline env
    evs = domEvents (envDom env)
    targets = if defKind d == KMixin then defIncludedBy d else [defName d]
    related a b = a == b || a `elem` ancestors env b || b `elem` ancestors env a

    handlerTypes =
      [ (T.drop 2 n, memHref m)
      | m <- defMembers d, memKind m == MAttribute, Just n <- [memName m]
      , maybe False isEventHandlerType (memType m), memberIncluded env d m ]

    handlerEvents = [ binding ty href | (ty, href) <- handlerTypes ]

    -- Types with a handler attribute here, on an ancestor or descendant, or
    -- on a mixin any of those include.  Extra event bindings are only made
    -- for the rest, so e.g. Element does not get a second `click` that
    -- would clash with GlobalEventHandlers.click (HTMLElement includes it).
    handled = S.fromList $ concat
      [ [ T.drop 2 n | m <- defMembers a, memKind m == MAttribute, Just n <- [memName m], maybe False isEventHandlerType (memType m) ]
      | an <- defName d : ancestors env (defName d) <> descendants env (defName d)
      , Just ad <- [M.lookup an (envDefs env)]
      , a <- ad : mapMaybe (`M.lookup` envDefs env) (defIncludes ad) ]

    extraEvents
      | defKind d /= KInterface || not (isEventTarget env (defName d)) = []
      | otherwise =
          [ binding ty (evHref ev)
          | ty <- nub [ evType ev | ev <- evs, any ((== defName d) . etTarget) (evTargets ev) ]
          , not (S.member ty handled)
          , ev <- take 1 [ ev | ev <- evs, evType ev == ty ] ]

    binding ty href = EventBinding
      { ebType = ty
      , ebHsName = hsName ty
      , ebInterface = eventIface ty
      , ebAsync = isAsync ty
      , ebHref = href
      }

    -- The event interface: for each target (the interfaces including a
    -- mixin, or the interface itself) find the interfaces events.json gives
    -- for related targets, then take the majority over targets (ties and
    -- disagreement within a target fall back to the least common
    -- supertype).  So GlobalEventHandlers.focus is FocusEvent (HTMLElement,
    -- SVGElement, MathMLElement) even though Window's focus is a plain Event.
    eventIface ty =
      let forTarget tgt = nub [ i | ev <- evs, evType ev == ty, Just i <- [evInterface ev]
                                  , any (\t -> related (etTarget t) tgt) (evTargets ev) ]
          perTarget = [ mostDerived is | tgt <- targets, let is = forTarget tgt, not (null is) ]
          counts = M.fromListWith (+) [ (i, 1 :: Int) | i <- perTarget ]
          best = [ i | (i, c) <- M.toList counts, c == maximum (M.elems counts) ]
          anyType = nub [ i | ev <- evs, evType ev == ty, Just i <- [evInterface ev] ]
      in if | [i] <- best -> i
            | not (null best) -> leastCommon best
            | otherwise -> leastCommon anyType

    -- Specs sometimes list a generic Event (e.g. HTML's focus on
    -- HTMLElement) alongside a specific one (UI Events' FocusEvent on
    -- Element).  When the candidates form a subtype chain use the most
    -- derived, as TypeScript's lib.dom does; otherwise the common supertype.
    mostDerived is =
      case [ i | i <- is, all (\j -> j == i || j `elem` ancestors env i) is ] of
        (i : _) -> i
        [] -> leastCommon is

    leastCommon [] = "Event"
    leastCommon (i : is) =
      fromMaybe "Event" $ listToMaybe
        [ a | a <- i : ancestors env i, all (\j -> a == j || a `elem` ancestors env j) is ]

    owners = defName d : defIncludedBy d
    oldFor ty = [ e | e <- blEvents bl, oeType e == ty ]
    oldHere ty = [ e | e <- oldFor ty, oeInterface e `elem` owners ]
    hsName ty = case oldHere ty <> mostCommon (oldFor ty) of
      (e : _) -> oeHsName e
      [] -> valueIdent ty
    mostCommon es = map snd . sortOn (Down . fst) . M.elems $
      M.fromListWith (\(a, e) (b, _) -> (a + b :: Int, e)) [ (oeHsName e, (1, e)) | e <- es ]
    isAsync ty = case oldHere ty <> oldFor ty of
      (e : _) -> oeAsync e
      [] -> ty `elem` ["readystatechange", "load", "error", "abort", "message", "progress", "loadend", "loadstart", "timeout"]
