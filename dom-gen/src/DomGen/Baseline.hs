-- | What the previous (domconv-webkit generated) bindings exposed.
--
-- @dom-gen baseline DIR OUT@ scans the old @JSDOM/Generated/*.hs@ modules
-- and writes a JSON summary.  The generator uses it to
--
--   * keep members that already existed and are still in the spec when BCD
--     support alone would drop them (avoids needless removals), and
--   * reuse the old Haskell names for overloads, constructors and events
--     (so e.g. @drawImageScaled@ and @keyDown@ keep their names).
module DomGen.Baseline
  ( Baseline (..)
  , OldFunction (..)
  , OldEvent (..)
  , extractBaseline
  , loadBaseline
  , oldHasMember
  ) where

import Control.Monad (forM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, dropExtension)

data OldFunction = OldFunction
  { ofInterface :: Text
  , ofJsName :: Text        -- ^ JS member name, or "constructor"
  , ofHsName :: Text
  , ofArgs :: [Text]        -- ^ Haskell argument names (without self)
  } deriving (Show)

data OldEvent = OldEvent
  { oeInterface :: Text
  , oeType :: Text          -- ^ JS event type, e.g. "keydown"
  , oeHsName :: Text        -- ^ e.g. "keyDown"
  , oeAsync :: Bool
  } deriving (Show)

data Baseline = Baseline
  { blMembers :: M.Map Text (S.Set Text)   -- ^ interface -> JS member names (plus "constructor", "on<event>", const names)
  , blFunctions :: [OldFunction]
  , blEvents :: [OldEvent]
  }

oldHasMember :: Baseline -> Text -> Text -> Bool
oldHasMember bl iface name = maybe False (S.member name) (M.lookup iface (blMembers bl))

------------------------------------------------------------------------------
-- Extraction

-- | @extractBaseline generatedDir out@
extractBaseline :: FilePath -> FilePath -> IO ()
extractBaseline dir out = do
  files <- sort . filter ((== ".hs") . takeExtension) <$> listDirectory dir
  results <- forM [ f | f <- files, f /= "Enums.hs" ] $ \f -> do
    src <- T.readFile (dir </> f)
    pure (extractModule (T.pack (dropExtension f)) src)
  let bl = Baseline
        { blMembers = M.fromList [ (i, ms) | (i, ms, _, _) <- results ]
        , blFunctions = concat [ fs | (_, _, fs, _) <- results ]
        , blEvents = concat [ es | (_, _, _, es) <- results ]
        }
  BL.writeFile out (AP.encodePretty' AP.defConfig { AP.confCompare = compare } bl <> "\n")
  putStrLn $ show (M.size (blMembers bl)) <> " interfaces, "
    <> show (sum (map S.size (M.elems (blMembers bl)))) <> " members, "
    <> show (length (blFunctions bl)) <> " functions, "
    <> show (length (blEvents bl)) <> " events"

-- | A top level equation: its head line and its body lines.
data Equation = Equation Text [Text]   -- ^ head line, body lines

equations :: [Text] -> [Equation]
equations = go
  where
    go [] = []
    go (l : rest)
      | isHead l =
          let (body, rest') = span (\x -> not (T.null x) && isSpace (T.head x)) rest
          in if any (("=" `T.isPrefixOf`) . T.stripStart) (take 1 body) || "=" `T.isInfixOf` l
               then Equation l body : go rest'
               else go rest'
      | otherwise = go rest
    isHead l = case T.uncons l of
      Just (c, _) -> (isAlphaNum c || c == '_') && not ("::" `T.isInfixOf` l)
        && not (any (`T.isPrefixOf` l) ["import ", "module ", "instance ", "pattern ", "newtype ", "data ", "class ", "type "])
      Nothing -> False

-- | Text between the first occurrence of @prefix@ and the next double quote.
quotedAfter :: Text -> Text -> [Text]
quotedAfter prefix t = case T.breakOn prefix t of
  (_, rest) | T.null rest -> []
            | otherwise ->
                let after = T.drop (T.length prefix) rest
                    (q, rest') = T.breakOn "\"" after
                in q : quotedAfter prefix rest'

extractModule :: Text -> Text -> (Text, S.Set Text, [OldFunction], [OldEvent])
extractModule iface src = (iface, members, functions, events)
  where
    ls = T.lines src
    whole = T.unwords (map T.strip ls)
    eqs = equations ls
    jsNames = concatMap (`quotedAfter` whole) ["^. jsf \"", "^. js \"", "^. jss \""]
    hasCtor = ("new (jsg \"" <> iface <> "\")") `T.isInfixOf` whole
    consts =
      [ T.takeWhile (\c -> isUpper c || c == '_' || isAlphaNum c) (T.drop 8 l)
      | l <- ls, "pattern " `T.isPrefixOf` l, " = " `T.isInfixOf` l ]
    events =
      [ OldEvent iface ty name ("unsafeEventNameAsync" `T.isInfixOf` body)
      | Equation h b <- eqs
      , let body = T.unwords (map T.strip (h : b))
            name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'') h
      , "unsafeEventName" `T.isInfixOf` body
      , ty <- take 1 (quotedAfter "toJSString \"" body)
      ]
    functions =
      [ OldFunction iface js name args
      | Equation h b <- eqs
      , let ws = T.words (T.takeWhile (/= '=') h)
      , (name : rawArgs) <- [ws]
      , not (any (`T.isSuffixOf` name) ["_", "Unsafe", "Unchecked"])
      , let body = T.unwords (map T.strip (h : b))
            args = filter (/= "self") rawArgs
      , js <- take 1 (quotedAfter "jsf \"" body
                       <> [ "constructor" | ("new (jsg \"" <> iface <> "\")") `T.isInfixOf` body ])
      ]
    members = S.fromList $ jsNames <> [ "constructor" | hasCtor ] <> consts
      <> [ "on" <> oeType e | e <- events ]

------------------------------------------------------------------------------
-- JSON

instance A.ToJSON Baseline where
  toJSON bl = A.object
    [ "members" .= M.map S.toAscList (blMembers bl)
    , "functions" .= [ [A.toJSON i, A.toJSON j, A.toJSON h, A.toJSON as] | OldFunction i j h as <- blFunctions bl ]
    , "events" .= [ [A.toJSON i, A.toJSON t, A.toJSON h, A.toJSON a] | OldEvent i t h a <- blEvents bl ]
    ]

instance A.FromJSON Baseline where
  parseJSON = A.withObject "baseline" $ \o -> do
    ms <- o .: "members"
    fs <- o .: "functions" >>= mapM (\(i, j, h, as) -> pure (OldFunction i j h as))
    es <- o .: "events" >>= mapM (\(i, t, h, a) -> pure (OldEvent i t h a))
    pure (Baseline (M.map S.fromList ms) fs es)

loadBaseline :: FilePath -> IO Baseline
loadBaseline path = BL.readFile path >>= either (fail . ((path <> ": ") <>)) pure . A.eitherDecode
