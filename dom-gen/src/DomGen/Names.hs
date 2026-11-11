-- | Haskell identifier helpers.
module DomGen.Names
  ( lowerFirst
  , upperFirst
  , camelWords
  , valueIdent
  , typeVar
  , conIdent
  , reservedValues
  , escapeReserved
  , freshName
  ) where

import Data.Char (isAlphaNum, isDigit, isLower, isUpper, toUpper)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

-- | Lower case the leading upper case run, keeping the last capital of a
-- run that starts a new word: @DOMContentLoaded@ -> @domContentLoaded@,
-- @HTMLElement@ -> @htmlElement@, @Foo@ -> @foo@.
lowerFirst :: Text -> Text
lowerFirst t =
  let (caps, rest) = T.span isUpper t
  in if | T.null caps -> t
        | T.length caps == 1 || T.null rest || not (isLower (T.head rest)) -> T.toLower caps <> rest
        | otherwise -> T.toLower (T.init caps) <> T.singleton (T.last caps) <> rest

upperFirst :: Text -> Text
upperFirst t = case T.uncons t of
  Just (c, r) -> T.cons (toUpper c) r
  Nothing -> t

-- | Join words separated by non alphanumeric characters in camel case:
-- @superscript-size-override@ -> @superscriptSizeOverride@.
camelWords :: Text -> Text
camelWords t = case filter (not . T.null) (T.split (not . isAlphaNum) t) of
  [] -> ""
  (w : ws) -> w <> T.concat (map upperFirst ws)

-- | A Haskell value identifier for an IDL name.
valueIdent :: Text -> Text
valueIdent t =
  let c = lowerFirst (camelWords t)
      c' = if maybe False (isDigit . fst) (T.uncons c) then "_" <> c else c
  in escapeReserved c'

-- | A type variable for a parameter name.
typeVar :: Text -> Text
typeVar = valueIdent

-- | A constructor (or type) identifier from an arbitrary string.
conIdent :: Text -> Text
conIdent t = T.concat (map upperFirst (filter (not . T.null) (T.split (not . isAlphaNum) t)))

-- | The first candidate not in @used@, falling back to numbered versions
-- of the first candidate (so it always succeeds).
freshName :: S.Set Text -> [Text] -> Text
freshName used candidates = go (candidates <> numbered)
  where
    base = case candidates of
      (c : _) -> c
      [] -> "x"
    numbered = [ base <> T.pack (show i) | i <- [(2 :: Int) ..] ]
    go (c : cs)
      | S.member c used = go cs
      | otherwise = c
    go [] = base  -- unreachable: numbered is infinite

escapeReserved :: Text -> Text
escapeReserved t
  | t `S.member` reservedValues = t <> "'"
  | otherwise = t

-- | Haskell keywords plus every value name the generated modules use
-- unqualified (so a generated binding or parameter never captures them).
reservedValues :: S.Set Text
reservedValues = S.fromList $
  [ "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import"
  , "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then"
  , "type", "where", "forall", "pattern", "proc", "rec", "mdo", "family", "role", "stock"
  , "anyclass", "via", "qualified", "hiding", "as"
  -- used unqualified by generated code
  , "self", "m", "result", "val", "callback", "args", "void", "liftDOM", "toJSVal", "toJSString"
  , "fromJSVal", "fromJSValUnchecked", "fromMaybeJSString", "fromJSArrayUnchecked", "valToBool"
  , "valToNumber", "js", "jss", "jsf", "jsg", "new", "function", "asyncFunction", "jsUndefined"
  , "readPromise", "unsafeEventName", "unsafeEventNameAsync", "maybeNullOrUndefined", "coerce"
  , "lift"
  ]
