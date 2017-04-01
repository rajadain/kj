module Main where

import Options
import Data.Maybe
import System.Process hiding (runCommand)
import Text.Printf

import Dir
import Parse
import System.Exit

data KjOptions = KjOptions { optListOnly :: Bool,
                             optDetailOnly :: Bool,
                             optAutoRestart :: Bool }

type RunMode = KjOptions -> [String] -> IO ()

instance Options KjOptions where
    defineOptions =
      pure KjOptions
      <*> mkViewOpt "list" (printf t "in machine readable format")
      <*> mkViewOpt "detail" (printf t "with docstring if available")
      <*> mkViewOpt "auto-restart" "automatically restart script when it terminates"
      where mkViewOpt long desc =
              defineOption optionType_bool
              (\o -> o { optionLongFlags = [long],
                         optionShortFlags = [head long],
                         optionDescription = desc,
                         optionDefault = False })
            t = "Print all available scripts (%s)"

main :: IO ()
main = runCommand $ \opts args ->
  let mode = if (optDetailOnly opts)
             then detailOnlyMode
             else if (optListOnly opts)
                  then listOnlyMode
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

scriptMode :: RunMode
scriptMode opts args =
  case args of
  [] -> putStrLn $ parsedHelp (parseOptions args::ParsedOptions KjOptions)
  (filename:scriptArgs) -> do
    files <- getFiles
    let needle = fromString filename
    let matches = fmap (take 1 . mapCompareExpand files) needle
    case matches of
      Nothing -> return ()
      (Just []) -> putStrLn "not found"
      (Just l) -> repeatProcess (optAutoRestart opts) (showLong . head $ l) scriptArgs

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
