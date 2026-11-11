-- | Backend independent module generation: naming, overloads, signatures,
-- variants (@foo@, @foo_@, @fooUnsafe@, @fooUnchecked@), constants, events
-- and module assembly.  A 'Backend' only decides how a 'Call' reaches
-- JavaScript and how results are converted.
module DomGen.Emit.Common
  ( -- * Backends
    Backend (..)
  , Call (..)
  , Target (..)
  , Access (..)
  , Arg (..)
  , ArgMode (..)
  , GenModule (..)
    -- * Emission
  , moduleDefinitions
  , emitModule
    -- * Helpers for backends
  , Ctx
  , hsTyCon
  , paren
  , paramTy
  , uniqueNames
  , callbackArgs
  , callbackArgTy
  , moduleNameFor
  ) where

import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import DomGen.Baseline
import DomGen.Env
import DomGen.Model
import DomGen.Names

data GenModule = GenModule
  { gmName :: Text          -- ^ module suffix, e.g. "Element"
  , gmText :: Text
  }

-- | What a binding does in JavaScript.
data Call = Call
  { cName :: Text           -- ^ Haskell name of the binding (backends derive helper names from it)
  , cTarget :: Target
  , cAccess :: Access
  , cArgs :: [Arg]
  , cResult :: HTy          -- ^ may be a 'HPromise'
  }

data Target
  = TSelf Text Text         -- ^ Haskell expression for the object and its newtype
  | TGlobal Text            -- ^ a global (interface object or namespace) by JS name

data Access
  = AMethod Text | AGet Text | ASet Text | ANew
  | ANewObject [Text]       -- ^ a plain object with these members, set from the call arguments in order

data ArgMode = Plain | Optional | Variadic
  deriving (Eq)

data Arg = Arg
  { aName :: Text           -- ^ Haskell variable
  , aTy :: HTy              -- ^ resolved IDL type (an optional argument is wrapped in 'Maybe' by the signature, not here)
  , aMode :: ArgMode
  }

data Backend = Backend
  { beRoot :: Text                                -- ^ "JSDOM" / "GHCJS.DOM"
  , beGenerated :: Text                           -- ^ prefix of generated modules
  , bePragmas :: [Text]
  , beImports :: [Text]
  , beCall :: Call -> ([Text], Text)
    -- ^ top level declarations needed (e.g. foreign imports) and an
    -- expression running the call.  Promises are already resolved, so its
    -- type corresponds to 'stripPromise' of the result.
  , beConvert :: HTy -> Text -> Text              -- ^ raw call result to the Haskell result
  , beUnchecked :: HTy -> Text -> Text            -- ^ for @HMaybe t@: raw result to @t@
  , beExtraExports :: [Text] -> [Text]            -- ^ extra exports given the module's declarations
  }

------------------------------------------------------------------------------

-- | Definitions that get a module (sorted).
moduleDefinitions :: Env -> [Definition]
moduleDefinitions env =
  [ d | n <- S.toAscList (envTyped env), Just d <- [M.lookup n (envDefs env)]
      , defKind d `elem` [KInterface, KMixin, KNamespace, KDictionary] ]

moduleNameFor :: Definition -> Text
moduleNameFor = upperFirst . defName

------------------------------------------------------------------------------
-- Rendering IDL types as Haskell types

-- | Constraints collected while rendering a signature.
type Ctx = [Text]

hsTyCon :: HTy -> Text
hsTyCon = \case
  HBool -> "Bool"
  HNum t -> t
  HAny -> "JSVal"
  HObject -> "GObject"
  HRecord -> "(Record JSString JSVal)"
  HRef n _ -> n
  HCallback n -> n
  HEnum n -> n
  HString -> "JSString"
  HUnit -> "()"
  HUnsupported n -> "JSVal {- " <> n <> " -}"
  HSeq t -> "[" <> hsTyCon t <> "]"
  HMaybe t -> "Maybe " <> paren (hsTyCon t)
  HPromise t -> hsTyCon t

paren :: Text -> Text
paren t
  | T.any (== ' ') t && not ("[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t) && not ("(" `T.isPrefixOf` t) = "(" <> t <> ")"
  | otherwise = t

-- | A parameter type; polymorphic where the old bindings were (strings and
-- types with an Is-class), using the parameter name as the type variable.
paramTy :: Text -> HTy -> (Text, Ctx)
paramTy v = \case
  HString -> (v, ["ToJSString " <> v])
  HRef _ (Just cls) -> (v, [cls <> " " <> v])
  HSeq t -> let (x, c) = paramTy v t in ("[" <> x <> "]", c)
  HMaybe t -> let (x, c) = paramTy v t in ("Maybe " <> paren x, c)
  HPromise _ -> ("JSVal", [])
  t -> (hsTyCon t, [])

-- | A result type; strings are polymorphic in @result@.
resultTy :: HTy -> (Text, Ctx)
resultTy = \case
  HString -> ("result", ["FromJSString result"])
  HSeq t -> let (x, c) = resultTy t in ("[" <> x <> "]", c)
  HMaybe t -> let (x, c) = resultTy t in ("Maybe " <> paren x, c)
  HPromise t -> resultTy t
  t -> (hsTyCon t, [])

returnsCallback :: HTy -> Bool
returnsCallback = \case
  HCallback _ -> True
  HMaybe t -> returnsCallback t
  HSeq t -> returnsCallback t
  HPromise t -> returnsCallback t
  _ -> False

stripPromise :: HTy -> HTy
stripPromise = \case
  HPromise t -> t
  t -> t

uniqueNames :: [Text] -> [Text]
uniqueNames = go S.empty
  where
    go _ [] = []
    go seen (n : ns) =
      let n' = freshName seen [n]
      in n' : go (S.insert n' seen) ns

------------------------------------------------------------------------------
-- Functions

data Fun = Fun
  { fName :: Text
  , fDoc :: Maybe Text
  , fCtx :: Ctx
  , fArgs :: [(Text, Text)]   -- ^ (argument name, type)
  , fResult :: Text
  , fBody :: Text             -- ^ expression of type DOM result
  }

renderFun :: Fun -> Text
renderFun Fun {..} = T.unlines $
  [ "-- | <" <> u <> ">" | Just u <- [fDoc] ] <>
  [ fName <> " :: " <> ctx <> T.intercalate " -> " (map snd fArgs <> ["m " <> paren fResult])
  , T.unwords (fName : map fst fArgs) <> " = liftDOM (" <> fBody <> ")"
  ]
  where
    ctx = "(" <> T.intercalate ", " (nub ("MonadDOM m" : fCtx)) <> ") => "

-- | The object a member is accessed on.
data Self = Self
  { selfArg :: Maybe (Text, Text)  -- ^ (name, type) unless static
  , selfCtx :: Ctx
  , selfTarget :: Target
  }

selfFor :: Env -> Definition -> Member -> Self
selfFor env d m
  | defKind d == KNamespace || memSpecial m == "static" = Self Nothing [] (TGlobal (defName d))
  | hasClass env dn = Self (Just ("self", "self")) [className dn <> " self"] (TSelf ("(Types.to" <> dn <> " self)") dn)
  | otherwise = Self (Just ("self", dn)) [] (TSelf "self" dn)
  where dn = defName d

-- | Haskell arguments (name, type), constraints and the call arguments.
argList :: Env -> [Argument] -> ([(Text, Text)], Ctx, [Arg])
argList env args =
  let names = uniqueNames (map (valueIdent . argName) args)
      one (a, n) =
        let ty0 = resolve env (argType a)
            optionalOnly = argOptional a && not (isMaybe ty0) && ty0 /= HAny
            mode | argVariadic a = Variadic
                 | optionalOnly = Optional
                 | otherwise = Plain
            sigTy = if optionalOnly then HMaybe ty0 else ty0
            (hty, c) = paramTy n sigTy
            hty' = if argVariadic a then "[" <> hty <> "]" else hty
        in ((n, hty'), c, Arg n ty0 mode)
      rendered = map one (zip args names)
  in ([ x | (x, _, _) <- rendered ], concat [ c | (_, c, _) <- rendered ], [ a | (_, _, a) <- rendered ])
  where
    isMaybe = \case HMaybe _ -> True; _ -> False

-- | Variants for a call: name, name_, nameUnsafe, nameUnchecked.
callVariants :: Backend -> Bool -> Maybe Text -> Self -> [(Text, Text)] -> Ctx -> Call -> ([Text], [Fun])
callVariants be withDiscard doc self args ctx call =
  let (decls, raw) = beCall be call
      ty = stripPromise (cResult call)
      name = cName call
      allArgs = maybe [] pure (selfArg self) <> args
      ctx' = selfCtx self <> ctx
      (rty, rctx) = resultTy ty
      main = Fun name doc (ctx' <> rctx) allArgs rty (beConvert be ty raw)
      discard = Fun (name <> "_") doc ctx' allArgs "()" ("void (" <> raw <> ")")
      unwrapped = case ty of HMaybe t -> t; t -> t
      (uty, uctx) = resultTy unwrapped
      unsafe = Fun (name <> "Unsafe") doc (ctx' <> ["HasCallStack"] <> uctx) allArgs uty
        ("(" <> beConvert be ty raw <> " >>= P.maybe (P.error \"Nothing to return\") P.return)")
      unchecked = Fun (name <> "Unchecked") doc (ctx' <> uctx) allArgs uty (beUnchecked be ty raw)
      funs = case ty of
        HUnit -> [main { fResult = "()", fBody = "void (" <> raw <> ")" }]
        HMaybe _ -> [main] <> [discard | withDiscard] <> [unsafe, unchecked]
        _ -> [main] <> [discard | withDiscard]
  in (decls, funs)

docLink :: Definition -> Member -> Maybe Text
docLink d m = case (memBcd m >>= cMdn, memHref m) of
  (Just u, _) -> Just (u <> " MDN " <> defName d <> "." <> fromMaybe "" (memName m) <> " documentation")
  (_, Just u) -> Just (u <> " " <> defName d <> "." <> fromMaybe "" (memName m) <> " specification")
  _ -> Nothing

------------------------------------------------------------------------------
-- Module emission

data Item = ItemFun [Text] Fun | ItemPattern Text Text | ItemEvent Text EventBinding

itemName :: Item -> Text
itemName = \case
  ItemFun _ f -> fName f
  ItemPattern n _ -> n
  ItemEvent n _ -> n

-- | Functions sharing declarations (the variants of one call) become
-- items; only the first carries the declarations.
funItems :: ([Text], [Fun]) -> [Item]
funItems (decls, funs) = zipWith (\i f -> ItemFun (if i == (0 :: Int) then decls else []) f) [0 ..] funs

emitModule :: Backend -> Env -> S.Set Text -> Definition -> GenModule
emitModule be env typesValues d = GenModule (moduleNameFor d) text
  where
    members = defMembersIncluded env d
    dn = defName d

    -- Constructors -------------------------------------------------------
    ctors = [ m | m <- members, memKind m == MConstructor ]
    ctorItems = concat
      [ funItems $ callVariants be False (docLink d m) (Self Nothing [] (TGlobal dn)) args ctx
          (Call name (TGlobal dn) ANew callArgs (HRef dn Nothing))
      | (m0, name0) <- overloadNames env d "constructor" ("new" <> dn) ctors
      , (m, name) <- keepOldArity m0 name0
      , let (args, ctx, callArgs) = argList env (memArguments m) ]

    -- Operations ---------------------------------------------------------
    ops = [ m | m <- members, memKind m == MOperation, isJust (memName m) ]
    opGroups = M.toList $ M.fromListWith (flip (<>))
      [ ((memSpecial m == "static", fromMaybe "" (memName m)), [m]) | m <- ops ]
    instanceOpNames = S.fromList [ n | ((False, n), _) <- opGroups ]
    opItems = concat
      [ funItems $ callVariants be True (docLink d m) self args ctx
          (Call name (selfTarget self) (AMethod js) callArgs ret)
      | ((static, js), ms) <- opGroups
      , let base0 = valueIdent js
            base = if static && S.member js instanceOpNames then base0 <> "Static" else base0
      , (m0, name0) <- overloadNames env d js base ms
      , (m, name) <- keepOldArity m0 name0
      , let self = selfFor env d m
            (args, ctx, callArgs) = argList env (memArguments m)
            ret0 = maybe HUnit (resolve env) (memType m)
            ret = if returnsCallback ret0 then HAny else ret0 ]

    -- When the old bindings had this operation with fewer arguments and the
    -- spec has since added only optional trailing ones (e.g.
    -- Document.createElement's options), keep the old name with the old
    -- arity and add the full version primed: createElement / createElement'.
    keepOldArity m name =
      -- (compared by count, not name: arguments get renamed, e.g.
      -- createElementNS's namespaceURI -> namespace)
      let olds = [ n
                 | f <- blFunctions (envBaseline env), ofInterface f `elem` (dn : defIncludedBy d)
                 , ofJsName f == fromMaybe "constructor" (memName m), ofHsName f == name
                 , let n = length (ofArgs f)
                 , n < length (memArguments m)
                 , all argOptional (drop n (memArguments m)) ]
      in case olds of
           (n : _) -> [(m { memArguments = take n (memArguments m) }, name), (m, name <> "'")]
           [] -> [(m, name)]

    -- Attributes ---------------------------------------------------------
    attrs0 = [ m | m <- members, memKind m == MAttribute, Just t <- [memType m], not (isEventHandlerType t) ]
    -- CSS declaration interfaces have both `margin-top` and `marginTop`; the
    -- dashed spellings are aliases and would get the same Haskell names.
    camelNames = S.fromList [ n | m <- attrs0, Just n <- [memName m], not ("-" `T.isInfixOf` n) ]
    attrs = [ m | m <- attrs0, maybe True (\n -> not ("-" `T.isInfixOf` n && S.member (camelWords n) camelNames)) (memName m) ]
    attrItems = concat
      [ setter <> getter
      | m <- attrs
      , Just js <- [memName m]
      , Just t <- [memType m]
      , let ty = resolve env t
            self = selfFor env d m
            cap = upperFirst (camelWords js)
            staticSuffix = if memSpecial m == "static" then "Static" else ""
            readonly = memReadonly m || hasExtAttr "PutForwards" (memExtAttrs m)
            (vty, vctx) = paramTy "val" ty
            setter =
              [ item
              | not readonly
              , item <- funItems $ callVariants be False (docLink d m) self [("val", vty)] vctx
                  (Call ("set" <> cap <> staticSuffix) (selfTarget self) (ASet js) [Arg "val" ty Plain] HUnit) ]
            getter
              | returnsCallback ty = []   -- callbacks cannot be read back from JS
              | otherwise = funItems $ callVariants be False (docLink d m) self [] []
                  (Call ("get" <> cap <> staticSuffix) (selfTarget self) (AGet js) [] ty) ]

    -- Constants ----------------------------------------------------------
    constItems =
      [ ItemPattern n lit
      | m <- members, memKind m == MConst, Just n <- [memName m]
      , maybe False (\(c, _) -> c >= 'A' && c <= 'Z') (T.uncons n)
      , Just lit <- [constLit =<< memValue m] ]

    -- Events -------------------------------------------------------------
    eventItems = [ ItemEvent (ebHsName b) b | b <- eventBindings env d ]
    renderEvent name b =
      let ety = ebInterface b
          sig | hasClass env dn = "(" <> className dn <> " self, IsEventTarget self) => EventName self " <> ety
              | otherwise = "EventName " <> dn <> " " <> ety
          mk = if ebAsync b then "unsafeEventNameAsync" else "unsafeEventName"
      in T.unlines $
           [ "-- | <" <> u <> " " <> ebType b <> " event>" | Just u <- [ebHref b] ] <>
           [ name <> " :: " <> sig
           , name <> " = " <> mk <> " (toJSString \"" <> ebType b <> "\")" ]

    -- Names: functions keep their names (primed on a clash); events and
    -- constants that clash get a suffix (as the old bindings did with e.g.
    -- closeEvent).
    -- Dictionaries ---------------------------------------------------------
    -- A dictionary is a plain JS object: new<Dict> makes one (taking the
    -- required members, inherited ones first), set<Member> / get<Member>
    -- reach the members (inherited ones through the Is-class, as for
    -- interfaces).  Getters of optional members are Maybe: an unset member
    -- reads as undefined.
    isDict = defKind d == KDictionary
    dictFields dd =
      [ (js, m, ty)
      | m <- defMembers dd, memKind m == MField, Just js <- [memName m], Just t <- [memType m]
      , let ty = resolve env t, usable ty ]
    usable = \case
      HUnsupported _ -> False
      HSeq t -> usable t
      HPromise t -> usable t
      HMaybe t -> usable t
      _ -> True
    lineage = reverse (dn : ancestors env dn)
    required =
      [ (js, ty) | a <- lineage, Just dd <- [M.lookup a (envDefs env)]
                 , (js, m, ty) <- dictFields dd, memRequired m ]
    dictCtorItems
      | not isDict = []
      | otherwise =
          let argNames = uniqueNames [ valueIdent js | (js, _) <- required ]
              rendered = [ (n, paramTy n ty, Arg n ty Plain) | ((_, ty), n) <- zip required argNames ]
          in funItems $ callVariants be False (defHref d >>= \u -> Just (u <> " " <> dn <> " specification"))
               (Self Nothing [] (TGlobal dn))
               [ (n, t) | (n, (t, _), _) <- rendered ] (concat [ c | (_, (_, c), _) <- rendered ])
               (Call ("new" <> dn) (TGlobal dn) (ANewObject (map fst required)) [ a | (_, _, a) <- rendered ] (HRef dn Nothing))
    dictMemberItems = concat
      [ setter <> getter
      | isDict
      , (js, m, ty) <- dictFields d
      , let self = selfFor env d m
            cap = upperFirst (camelWords js)
            (vty, vctx) = paramTy "val" ty
            isMaybe = case ty of HMaybe _ -> True; _ -> False
            gty = if memRequired m || isMaybe || ty == HAny then ty else HMaybe ty
            setter = funItems $ callVariants be False (docLink d m) self [("val", vty)] vctx
              (Call ("set" <> cap) (selfTarget self) (ASet js) [Arg "val" ty Plain] HUnit)
            getter
              | returnsCallback ty = []   -- callbacks cannot be read back from JS
              | otherwise = funItems $ callVariants be False (docLink d m) self [] []
                  (Call ("get" <> cap) (selfTarget self) (AGet js) [] gty) ]

    named = dedupe (dictCtorItems <> ctorItems <> opItems <> attrItems <> dictMemberItems) (constItems <> eventItems)
    dedupe primary secondary =
      let primary' = uniqueFuns primary
          used0 = S.fromList (map itemName primary')
          step (used, acc) it =
            let n' = freshName used [itemName it, itemName it <> "Event"]
            in (S.insert n' used, acc <> [rename n' it])
      in primary' <> snd (foldl step (used0, []) secondary)
    rename n = \case
      ItemEvent _ b -> ItemEvent n b
      ItemPattern _ v -> ItemPattern n v
      i -> i
    uniqueFuns = go S.empty
      where
        go _ [] = []
        go seen (ItemFun ds f : rest)
          | S.member (fName f) seen = go seen (ItemFun ds f { fName = fName f <> "'" } : rest)
          | otherwise = ItemFun ds f : go (S.insert (fName f) seen) rest
        go seen (i : rest) = i : go (S.insert (itemName i) seen) rest

    decls = concat [ ds | ItemFun ds _ <- named ]
    exportsOf = \case
      ItemPattern n _ -> "pattern " <> n
      i -> itemName i
    typeExports
      | defKind d == KNamespace = []
      | hasClass env dn = [dn <> "(..)", "gType" <> dn, className dn, "to" <> dn]
      | otherwise = [dn <> "(..)", "gType" <> dn]
    exports = beExtraExports be decls <> map exportsOf named <> typeExports

    hiding = [ n | n <- map itemName named, S.member n typesValues ]

    body = T.intercalate "\n" (map renderItem named)
    renderItem = \case
      ItemFun ds f -> T.concat [ x <> "\n" | x <- ds ] <> renderFun f
      ItemPattern n v -> "pattern " <> n <> " = " <> v <> "\n"
      ItemEvent n b -> renderEvent n b

    typesModule = beRoot be <> ".Types"
    text = T.unlines
      (bePragmas be <>
      [ "-- | Generated by dom-gen from " <> defSpec d <> " (webref). Do not edit."
      , "module " <> beGenerated be <> "." <> moduleNameFor d
      , "       (" <> T.intercalate ", " exports <> ")"
      , "       where"
      ]) <> T.unlines (beImports be)
        <> "import " <> typesModule <> (if null hiding then "" else " hiding (" <> T.intercalate ", " hiding <> ")") <> "\n"
        <> "import qualified " <> typesModule <> " as Types\n"
        <> "import " <> beRoot be <> ".EventTargetClosures (EventName, unsafeEventName, unsafeEventNameAsync)\n"
        <> "import " <> beGenerated be <> ".Enums\n\n"
        <> body

constLit :: Value -> Maybe Text
constLit = \case
  VNumber n
    | "-" `T.isPrefixOf` n -> Just ("(" <> n <> ")")
    | n `elem` ["Infinity", "-Infinity", "NaN"] -> Nothing
    | otherwise -> Just n
  VBool True -> Just "True"
  VBool False -> Just "False"
  _ -> Nothing

-- | Names for an overload set.  An overload whose argument names match an
-- old binding keeps the old name; the rest take the base name (if free)
-- or the base name plus the names of the arguments that distinguish it.
overloadNames :: Env -> Definition -> Text -> Text -> [Member] -> [(Member, Text)]
overloadNames env d js base ms = case ms of
  [m] -> [(m, base)]
  _ ->
    let owners = defName d : defIncludedBy d
        argNames m = map (valueIdent . argName) (memArguments m)
        olds = [ f | f <- blFunctions (envBaseline env), ofInterface f `elem` owners, ofJsName f == js ]
        -- an old binding with the same argument names, or failing that the
        -- only old binding with the same number of arguments (arguments
        -- get renamed, e.g. XMLHttpRequest.open's user -> username)
        old m = case [ ofHsName f | f <- olds, ofArgs f == argNames m ] of
          [] -> case [ ofHsName f | f <- olds, length (ofArgs f) == length (memArguments m) ] of
                  [n] -> [n]
                  _ -> []
          ns -> ns
        first = case ms of (m : _) -> argNames m; [] -> []
        derived m =
          let extra = [ upperFirst a | a <- argNames m, a `notElem` first ]
          in if null extra then [base <> "'"] else [base <> T.concat extra]
        step (used, acc) (i, m) =
          let candidates = take 1 (old m) <> [base | i == (1 :: Int)] <> derived m
              n = freshName used candidates
          in (S.insert n used, M.insert i n acc)
        -- assign old names first so a new overload cannot take them
        ordered = sortOn (\(_, m) -> null (old m)) (zip [1 ..] ms)
        (_, named) = foldl step (S.empty, M.empty) ordered
    in [ (m, named M.! i) | (i, m) <- zip [1 ..] ms ]

------------------------------------------------------------------------------
-- Callbacks

-- | Argument names, types (concrete) and the spec of a callback or
-- callback interface.
callbackArgs :: Env -> Text -> ([Text], [HTy], Text)
callbackArgs env n = (names, tys, spec)
  where
    dom = envDom env
    (args, spec) = case M.lookup n (domCallbacks dom) of
      Just cb -> (cbArguments cb, cbSpec cb)
      Nothing -> case M.lookup n (envDefs env) of
        Just d -> ( concat (take 1 [ memArguments m | m <- defMembers d, memKind m == MOperation ]), defSpec d )
        Nothing -> ([], "")
    names = uniqueNames (map (valueIdent . argName) args)
    tys = [ callbackArgTy (resolve env (argType a)) | a <- args ]

-- | Callback arguments are concrete: strings as JSString, Is-class types as
-- their newtype.
callbackArgTy :: HTy -> HTy
callbackArgTy = \case
  HRef r _ -> HRef r Nothing
  HSeq t -> HSeq (callbackArgTy t)
  HMaybe t -> HMaybe (callbackArgTy t)
  HPromise _ -> HAny
  HUnit -> HAny
  t -> t
