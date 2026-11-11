module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension)

import DomGen.ApiDiff (apiDiff)
import DomGen.Baseline (Baseline (..), OldFunction (..), extractBaseline, loadBaseline)
import DomGen.Names (valueIdent)
import DomGen.Emit.Cabal
import DomGen.Emit.Enums
import DomGen.Emit.Existence (emitExistence)
import DomGen.Emit.Common (GenModule (..), emitModule, moduleDefinitions)
import DomGen.Emit.JSaddle
import DomGen.Emit.JavaScript
import DomGen.Emit.Types
import DomGen.Env
import DomGen.Load (loadDom)
import DomGen.Model

usage :: String
usage = unlines
  [ "usage: dom-gen generate INPUTS BASELINE.json JSADDLE_DOM_DIR GHCJS_DOM_DIR"
  , "       dom-gen stats INPUTS BASELINE.json"
  , "       dom-gen baseline OLD_GENERATED_DIR OUT.json"
  , "       dom-gen apidiff INPUTS BASELINE.json OLD.hoogle.txt NEW.hoogle.txt OUT.md"
  , "       dom-gen existence INPUTS BASELINE.json OUT.hs"
  , ""
  , "  INPUTS  directory built by `nix build .#dom-gen-inputs`"
  ]

main :: IO ()
main = getArgs >>= \case
  ["generate", inputs, baseline, jsaddleDom, ghcjsDom] -> generate inputs baseline jsaddleDom ghcjsDom
  ["stats", inputs, baseline] -> stats inputs baseline
  ["existence", inputs, baseline, out] -> loadEnv inputs baseline >>= writeIfChanged out . emitExistence
  ["baseline", dir, out] -> extractBaseline dir out
  ["apidiff", inputs, baseline, oldHoogle, newHoogle, out] -> do
    env <- loadEnv inputs baseline
    apiDiff env oldHoogle newHoogle out
  _ -> die usage

loadEnv :: FilePath -> FilePath -> IO Env
loadEnv inputs baseline = buildEnv <$> loadDom inputs <*> loadBaseline baseline

-- | Write a file only when its content changes (keeps rebuilds small).
writeIfChanged :: FilePath -> T.Text -> IO ()
writeIfChanged path new = do
  exists <- doesFileExist path
  old <- if exists then Just <$> T.readFile path else pure Nothing
  when (old /= Just new) $ T.writeFile path new

editFile :: FilePath -> (T.Text -> Either String T.Text) -> IO ()
editFile path f = T.readFile path >>= either die (writeIfChanged path) . f

-- | Write the generated modules (and Enums) to @genDir@, removing any
-- module there that is no longer generated.  Returns the module suffixes.
writeGenerated :: FilePath -> [GenModule] -> T.Text -> IO (S.Set T.Text)
writeGenerated genDir modules enums = do
  let names = S.fromList (map gmName modules <> ["Enums"])
  createDirectoryIfMissing True genDir
  existing <- filter ((== ".hs") . takeExtension) <$> listDirectory genDir
  forM_ existing $ \f ->
    when (not (S.member (T.pack (dropExtension f)) names)) $ removeFile (genDir </> f)
  forM_ modules $ \m -> writeIfChanged (genDir </> T.unpack (gmName m) <.> "hs") (gmText m)
  writeIfChanged (genDir </> "Enums.hs") enums
  putStrLn $ "wrote " <> show (S.size names) <> " modules to " <> genDir
  pure names

-- | Hand written modules (by suffix) in a directory, if it exists.
handWritten :: FilePath -> IO [T.Text]
handWritten dir = do
  exists <- doesDirectoryExist dir
  if exists
    then S.toAscList . S.fromList . map (T.pack . dropExtension) . filter ((== ".hs") . takeExtension) <$> listDirectory dir
    else pure []

-- | @generate INPUTS BASELINE JSADDLE_DOM_DIR GHCJS_DOM_DIR@
generate :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
generate inputs baseline jsaddleDom ghcjsDom = do
  env <- loadEnv inputs baseline
  let typesValues = typesValueNames env

  -- jsaddle-dom (and ghcjs-dom-jsaddle / ghcjs-dom module lists)
  let srcDir = jsaddleDom </> "src" </> "JSDOM"
  names <- writeGenerated (srcDir </> "Generated")
    (map (emitModule jsaddleBackend env typesValues) (moduleDefinitions env)
       <> map (emitCallbackModule env) (S.toAscList (envCallbacks env)))
    (emitEnums JSaddle env)
  editFile (srcDir </> "Types.hs") (spliceTypes JSaddle env)
  custom <- handWritten (srcDir </> "Custom")
  let public = S.toAscList (names <> S.fromList custom)
  editFile (jsaddleDom </> "jsaddle-dom.cabal") (spliceJSaddleDomCabal custom (S.toAscList names))
  editFile (ghcjsDom </> "ghcjs-dom-jsaddle" </> "ghcjs-dom-jsaddle.cabal") (spliceGhcjsDomJSaddleCabal public)
  writeIfChanged (ghcjsDom </> "ghcjs-dom-jsaddle" </> "reexported-modules.txt") (ghcjsDomJSaddleReexports public)
  editFile (ghcjsDom </> "ghcjs-dom" </> "ghcjs-dom.cabal") (spliceGhcjsDomCabal public)

  -- ghcjs-dom-javascript (GHC JavaScript backend; no jsaddle)
  let jsPkg = ghcjsDom </> "ghcjs-dom-javascript"
      jsSrc = jsPkg </> "src" </> "GHCJS" </> "DOM"
  jsNames <- writeGenerated (jsSrc </> "JSFFI" </> "Generated")
    (map (emitModule javascriptBackend env typesValues) (moduleDefinitions env)
       <> map (emitCallbackModuleJS env) (S.toAscList (envCallbacks env)))
    (emitEnums JSFFI env)
  editFile (jsSrc </> "Types.hs") (spliceTypes JSFFI env)
  jsCustom <- filter (/= "Generated") <$> handWritten (jsSrc </> "JSFFI")
  editFile (jsPkg </> "ghcjs-dom-javascript.cabal") (spliceGhcjsDomJavaScriptCabal jsCustom (S.toAscList jsNames))
  writeIfChanged (jsPkg </> "reexported-modules.txt") (ghcjsDomJavaScriptReexports jsCustom (S.toAscList jsNames))

stats :: FilePath -> FilePath -> IO ()
stats inputs baselinePath = do
  dom <- loadDom inputs
  bl <- loadBaseline baselinePath
  let defs = M.elems (domDefinitions dom)
      count k = length (filter ((== k) . defKind) defs)
      members = concatMap defMembers defs
      withBcd = length (filter (isJust . memBcd) members)
  putStrLn $ "interfaces " <> show (count KInterface)
    <> ", mixins " <> show (count KMixin)
    <> ", namespaces " <> show (count KNamespace)
    <> ", dictionaries " <> show (count KDictionary)
    <> ", callback interfaces " <> show (count KCallbackInterface)
  putStrLn $ "enums " <> show (M.size (domEnums dom))
    <> ", typedefs " <> show (M.size (domTypedefs dom))
    <> ", callbacks " <> show (M.size (domCallbacks dom))
    <> ", events " <> show (length (domEvents dom))
    <> ", elements " <> show (length (domElements dom))
  putStrLn $ "members " <> show (length members) <> ", with BCD data " <> show withBcd
  let env = buildEnv dom bl
      typedDefs = [ d | n <- S.toList (envTyped env), Just d <- [M.lookup n (envDefs env)] ]
      byKind k = length (filter ((== k) . defKind) typedDefs)
      included = sum [ length (defMembersIncluded env d) | d <- typedDefs, defKind d /= KDictionary ]
      oldIfaces = M.keysSet (blMembers bl)
      newIfaces = S.fromList [ defName d | d <- typedDefs, defKind d `elem` [KInterface, KMixin, KNamespace] ]
                  <> envCallbacks env
  putStrLn $ "selected: interfaces " <> show (byKind KInterface) <> ", mixins " <> show (byKind KMixin)
    <> ", namespaces " <> show (byKind KNamespace) <> ", dictionaries " <> show (byKind KDictionary)
  putStrLn $ "unions " <> show (M.size (envUnions env)) <> ", enums " <> show (S.size (envEnums env))
    <> ", callbacks " <> show (S.size (envCallbacks env)) <> ", included members " <> show included
  putStrLn $ "old modules no longer generated (" <> show (S.size (oldIfaces S.\\ newIfaces)) <> "): "
    <> unwords (map T.unpack (S.toList (oldIfaces S.\\ newIfaces)))
  -- Operations that gained trailing optional arguments since the old
  -- bindings (old argument names are a strict prefix of the new ones and
  -- every extra argument is optional).
  let grown =
        [ (defName d, js, ofArgs f, newArgs)
        | d <- typedDefs, m <- defMembersIncluded env d, memKind m == MOperation
        , Just js <- [memName m]
        , let newArgs = map (valueIdent . argName) (memArguments m)
        , f <- blFunctions bl, ofInterface f `elem` (defName d : defIncludedBy d), ofJsName f == js
        , let n = length (ofArgs f)
        , n < length newArgs, ofArgs f == take n newArgs
        , all argOptional (drop n (memArguments m)) ]
  putStrLn $ "operations that gained trailing optional arguments: " <> show (length grown)
  mapM_ (\(i, js, o, n) -> putStrLn ("  " <> T.unpack (i <> "." <> js <> " (" <> T.intercalate ", " o <> ") -> (" <> T.intercalate ", " n <> ")"))) grown
