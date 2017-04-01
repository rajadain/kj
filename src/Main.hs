module Main where

import Options
import Data.Maybe
import System.Process hiding (runCommand)
import Text.Printf

import Dir
import Parse
import System.Exit

data KjOptions = KjOptions { optListOnly :: Bool
                           , optDetailOnly :: Bool
                           , optCatFile :: Bool
                           , optAutoRestart :: Bool }

type RunMode = KjOptions -> [String] -> IO ()

instance Options KjOptions where
    defineOptions =
      pure KjOptions
      <*> mkViewOpt "list" "Print all available scripts (in machine readable format)"
      <*> mkViewOpt "detail" "Print all available scripts (with docstring if available)"
      <*> mkViewOpt "cat" "display contents of script"
      <*> mkViewOpt "auto-restart" "automatically restart script when it terminates"
      where mkViewOpt long desc =
              defineOption optionType_bool
              (\o -> o { optionLongFlags = [long],
                         optionShortFlags = [head long],
                         optionDescription = desc,
                         optionDefault = False })

main :: IO ()
main = runCommand $ \opts args ->
  let mode = if (optDetailOnly opts)
             then detailOnlyMode
             else if (optListOnly opts)
                  then listOnlyMode
                  else if (optCatFile opts)
                       then catFileMode
                       else scriptMode
  in mode opts args

listOnlyMode :: RunMode
listOnlyMode _ _ = getFiles >>= mapM_ (putStrLn . showShort)

detailOnlyMode :: RunMode
detailOnlyMode _ _ = do
  files <- getFiles
  texts <- mapM (readFile . showLong) files
  let parsed = zipWith parseScript (map fileName files) texts
  mapM_ print (catMaybes parsed)


withFileMode :: (String -> [String] -> IO ()) -> RunMode
withFileMode f _ args =
  case args of
  [] -> putStrLn $ parsedHelp (parseOptions args::ParsedOptions KjOptions)
  (filename:scriptArgs) -> do
    files <- getFiles
    let needle = fromString filename
    let matches = fmap (take 1 . mapCompareExpand files) needle
    case matches of
      Nothing -> return ()
      (Just []) -> putStrLn "not found"
      (Just l) -> f (showLong . head $ l) scriptArgs

catFileMode :: RunMode
catFileMode opts args = withFileMode f opts args
  where f p _ = putStr =<< readFile p

scriptMode :: RunMode
scriptMode opts = withFileMode (\f args' -> repeatProcess (optAutoRestart opts) f args') opts

repeatProcess :: Bool -> FilePath -> [String] -> IO ()
repeatProcess bool cmd args = do
  process <- spawnProcess cmd args
  code <- waitForProcess process
  case (bool, code) of
    (False, _) -> return ()
    (True, ExitSuccess) -> return ()
    (True, ExitFailure i) -> do
      printf "KJ WARNING: program exited with status code %d ... RESTARTING\n" i
      repeatProcess bool cmd args
