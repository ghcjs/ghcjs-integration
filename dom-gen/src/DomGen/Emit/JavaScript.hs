-- | The GHC JavaScript backend (ghcjs-dom-javascript): one typed
-- @foreign import javascript@ per member, with the member name compiled
-- into the JavaScript.  Depends only on ghcjs-base (not jsaddle).
--
-- Marshalling: strings travel as 'JSString', booleans and numbers as
-- themselves, JSVal newtypes (interfaces, dictionaries, unions) as
-- themselves, nullable / optional strings as @Nullable@ / @Optional
-- JSString@; anything else (sequences, enums, records, Int53, other
-- nullable values) as a 'JSVal' through ToJSVal / FromJSVal.
module DomGen.Emit.JavaScript
  ( javascriptBackend
  , emitCallbackModuleJS
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import DomGen.Emit.Common
import DomGen.Env

javascriptBackend :: Backend
javascriptBackend = Backend
  { beRoot = "GHCJS.DOM"
  , beGenerated = "GHCJS.DOM.JSFFI.Generated"
  , bePragmas =
      [ "{-# LANGUAGE PatternSynonyms #-}"
      , "{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}"
      , "{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}"
      , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
      ]
  , beImports = standardImports
  , beCall = call
  , beConvert = convert
  , beUnchecked = unchecked
  , beExtraExports = foreignNames
  }

standardImports :: [Text]
standardImports =
  [ "import Prelude ((.), (==), (>>=), (<$>), (++), Bool(..), Maybe(..), IO, Int, Word, Double, Float)"
  , "import qualified Prelude as P"
  , "import Control.Monad (void)"
  , "import GHCJS.Types (JSVal, JSString)"
  , "import GHCJS.Foreign (jsUndefined, jsNull)"
  ]

-- | Names of the foreign imports among a module's declarations (exported,
-- as the old jsffi bindings did).
foreignNames :: [Text] -> [Text]
foreignNames decls =
  [ T.takeWhile (/= ' ') (T.strip l)
  | d <- decls, "foreign import" `T.isPrefixOf` d
  , l <- drop 1 (T.lines d), " :: " `T.isInfixOf` l ]

-- | Is the Haskell type for this a newtype over JSVal (so it can be passed
-- to and returned from a foreign import directly)?
jsvalNewtype :: HTy -> Bool
jsvalNewtype = \case
  HRef n _ -> n `notElem` ["Int53", "Word53"]
  HObject -> True
  _ -> False

-- | Foreign type of a result.
ffiResult :: HTy -> Text
ffiResult = \case
  HUnit -> "()"
  HBool -> "Bool"
  HNum t -> t
  HString -> "JSString"
  HAny -> "JSVal"
  t | jsvalNewtype t -> hsTyCon t
    | otherwise -> "JSVal"

-- | A foreign argument: its type and how to compute it from the Haskell
-- argument (pure, or an IO action).
data FArg = FArg { faTy :: Text, faExpr :: Either Text Text, faSpread :: Bool }

ffiArg :: Arg -> FArg
ffiArg (Arg v ty mode) = case mode of
  Variadic -> FArg "JSVal" (Right ("toJSVal " <> v)) True
  Optional -> case ty of
    HString -> FArg "(Optional JSString)" (Left ("toOptionalJSString " <> v)) False
    _ -> FArg "JSVal" (Right ("P.maybe (P.return jsUndefined) toJSVal " <> v)) False
  Plain -> case ty of
    HString -> FArg "JSString" (Left ("toJSString " <> v)) False
    HBool -> FArg "Bool" (Left v) False
    HNum t -> FArg t (Left v) False
    HAny -> FArg "JSVal" (Left v) False
    HRef n (Just _) | jsvalNewtype ty -> FArg n (Left ("Types.to" <> n <> " " <> v)) False  -- qualified: a module may define its own toX method
    _ | jsvalNewtype ty -> FArg (hsTyCon ty) (Left v) False
    HMaybe HString -> FArg "(Nullable JSString)" (Left ("toMaybeJSString " <> v)) False
    _ -> FArg "JSVal" (Right ("toJSVal " <> v)) False

call :: Call -> ([Text], Text)
call Call {..} = ([decl], hsCall)
  where
    ffiName = "js_" <> cName
    isPromise = case cResult of HPromise _ -> True; _ -> False
    result = case cResult of HPromise t -> t; t -> t

    selfArgs = case cTarget of
      TSelf e ty -> [(FArg ty (Left e) False)]
      TGlobal _ -> []
    fargs = selfArgs <> map ffiArg cArgs
    params = [ "$" <> T.pack (show i) | i <- [1 .. length fargs] ]
    paramFor (p, fa) = if faSpread fa then "..." <> p else p
    (selfParam, argParams) = case cTarget of
      TSelf _ _ -> (head' params, map paramFor (drop 1 (zip params fargs)))
      TGlobal g -> ("globalThis[\"" <> g <> "\"]", map paramFor (zip params fargs))
    head' = \case (p : _) -> p; [] -> "$1"
    argsJs = T.intercalate ", " argParams
    expr = case cAccess of
      AMethod js -> selfParam <> "[\"" <> js <> "\"](" <> argsJs <> ")"
      AGet js -> selfParam <> "[\"" <> js <> "\"]"
      ASet js -> selfParam <> "[\"" <> js <> "\"] = " <> argsJs
      ANew -> "new " <> selfParam <> "(" <> argsJs <> ")"
      ANewObject names -> "({" <> T.intercalate ", " [ T.pack (show (T.unpack n)) <> ": " <> p | (n, p) <- zip names argParams ] <> "})"
    lambdaParams = T.intercalate ", " (params <> [ "$c" | isPromise ])
    bodyJs
      | isPromise = "return " <> expr <> ".then(function(s) { $c(null, s);}, function(e) { $c(e, null);});"
      | otherwise = case cAccess of
          ASet _ -> expr <> ";"
          _ -> "return " <> expr <> ";"
    js = "((" <> lambdaParams <> ") => { " <> bodyJs <> " })"
    -- Getters cannot reasonably throw; operations, constructors and setters
    -- can (e.g. querySelector with a bad selector), so they are "safe" and
    -- the JavaScript exception becomes a Haskell one.
    safety
      | isPromise = "interruptible"
      | otherwise = case cAccess of
          AGet _ -> "unsafe"
          _ -> "safe"
    ret | isPromise = "(JSVal, " <> ffiResult result <> ")"
        | otherwise = ffiResult result
    sig = T.intercalate " -> " (map faTy fargs <> ["IO " <> paren ret])
    decl = "foreign import javascript " <> safety <> " " <> T.pack (show (T.unpack js))
           <> "\n        " <> ffiName <> " :: " <> sig

    -- monadic arguments are bound first
    binds = [ (e, "a" <> T.pack (show i) <> "'") | (i, fa) <- zip [(1 :: Int) ..] fargs, Right e <- [faExpr fa] ]
    actual = snd (foldl pick (binds, []) fargs)
    pick (bs, acc) fa = case faExpr fa of
      Left e -> (bs, acc <> ["(" <> e <> ")"])
      Right _ -> case bs of
        ((_, v) : rest) -> (rest, acc <> [v])
        [] -> (bs, acc)
    invoke = T.unwords (ffiName : actual)
    bound = foldr (\(e, v) inner -> "(" <> e <> ") >>= \\" <> v <> " -> " <> inner) invoke binds
    hsCall
      | isPromise = "((" <> bound <> ") >>= checkPromiseResult)"
      | otherwise = "(" <> bound <> ")"

-- | Raw foreign result to the Haskell result.
convert :: HTy -> Text -> Text
convert ty e = case ty of
  HUnit -> "(void " <> e <> ")"
  HBool -> e
  HNum _ -> e
  HAny -> e
  HString -> "(fromJSString <$> " <> e <> ")"
  HMaybe HString -> "((fromMaybeJSString . Nullable) <$> " <> e <> ")"
  t | jsvalNewtype t -> e
  -- JSVal results; for `Maybe a` the Maybe instance maps null/undefined to Nothing
  _ -> "(" <> e <> " >>= fromJSValUnchecked)"

-- | Raw (nullable, so JSVal) result to the unwrapped type, unchecked.
unchecked :: HTy -> Text -> Text
unchecked ty e = case ty of
  HMaybe (HSeq _) -> "(" <> e <> " >>= fromJSValUnchecked)"
  HMaybe (HCallback _) -> "(" <> e <> " >>= fromJSValUnchecked)"
  _ -> "(pFromJSVal <$> " <> e <> ")"

-- | @newFoo@ (sync, continuing asynchronously if it blocks), @newFooSync@
-- (throws if it blocks) and @newFooAsync@ for a callback.  The callback's
-- return value is ignored.
emitCallbackModuleJS :: Env -> Text -> GenModule
emitCallbackModuleJS env n = GenModule n text
  where
    (names, tys, spec) = callbackArgs env n
    arity = length names
    suffix = if arity == 0 then "" else T.pack (show arity)
    fnTy = T.intercalate " -> " (map hsTyCon tys <> ["IO ()"])
    lam
      | arity == 0 = "callback"
      | otherwise = "(\\ " <> T.unwords names <> " -> do"
          <> T.concat [ "\n          " <> v <> "' <- " <> fromJS t v | (v, t) <- zip names tys ]
          <> "\n          callback " <> T.unwords (map (<> "'") names) <> ")"
    mk sfx fn = T.unlines
      [ "new" <> n <> sfx <> " :: (MonadDOM m) => (" <> fnTy <> ") -> m " <> n
      , "new" <> n <> sfx <> " callback = liftDOM ((" <> n <> " . coerce) <$> " <> fn <> " " <> lam <> ")"
      ]
    exports = ["new" <> n, "new" <> n <> "Sync", "new" <> n <> "Async", n]
    text = T.unlines
      [ "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
      , "-- | Generated by dom-gen from " <> spec <> " (webref). Do not edit."
      , "module GHCJS.DOM.JSFFI.Generated." <> n
      , "       (" <> T.intercalate ", " exports <> ")"
      , "       where"
      ] <> T.unlines standardImports
        <> "import Data.Coerce (coerce)\n"
        <> "import GHC.JS.Foreign.Callback (OnBlocked(..), syncCallback" <> suffix <> ", asyncCallback" <> suffix <> ")\n"
        <> "import GHCJS.DOM.Types\nimport GHCJS.DOM.JSFFI.Generated.Enums\n\n"
        <> T.intercalate "\n"
             [ mk "" ("syncCallback" <> suffix <> " ContinueAsync")
             , mk "Sync" ("syncCallback" <> suffix <> " ThrowWouldBlock")
             , mk "Async" ("asyncCallback" <> suffix) ]

fromJS :: HTy -> Text -> Text
fromJS t v = case t of
  HAny -> "P.return " <> v
  _ -> "fromJSValUnchecked " <> v   -- for Maybe: null/undefined -> Nothing
