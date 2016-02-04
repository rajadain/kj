module Main where

import System.Directory
import Options
import Data.Maybe
import System.Process hiding (runCommand)
import System.Environment (getArgs)
import Text.Printf

import Dir
import Parse

data KjOptions = KjOptions { optListOnly :: Bool, optDetailOnly :: Bool }

instance Options KjOptions where
    defineOptions =
      pure KjOptions
      <*> mkViewOpt "list" "in machine readable format"
      <*> mkViewOpt "detail" "with docstring if available"
      where mkViewOpt long desc =
              defineOption optionType_bool
              (\o -> o { optionLongFlags = [long],
                         optionShortFlags = [head long],
                         optionDescription = printf t desc,
                         optionDefault = False })
            t = "Print all available scripts (%s)"

firstArgIsScript :: [String] -> Bool
firstArgIsScript args =
  case args of [] -> False
               x:_ -> head x /= '-'

-- parse arguments, determine runmode, call runmode
main :: IO ()
main = do
  argv <- getArgs
  if firstArgIsScript argv
    then scriptMode argv
    else runCommand $ \opts args -> do
    let detailOnly = optDetailOnly opts
        listOnly = optListOnly opts
        hasScriptArgs = not (null args)
        modes = filter (==True) [detailOnly, listOnly, hasScriptArgs]
        hasConflictingParams = 1 /= length modes
    if hasConflictingParams
      then putStrLn (parsedHelp (parseOptions argv::ParsedOptions KjOptions))
      else if detailOnly
           then detailOnlyMode
           else listOnlyMode

-- prints all available scripts
listOnlyMode :: IO ()
listOnlyMode = getFiles >>= mapM_ (putStrLn . showShort)

-- prints all available scripts with their summaries
detailOnlyMode :: IO ()
detailOnlyMode = do
  files <- getFiles
  texts <- mapM (readFile . showLong) files
  let parsed = zipWith parseScript (map fileName files) texts
  mapM_ print (catMaybes parsed)

-- the main runmode, executes a script
scriptMode :: [String] -> IO ()
scriptMode outerArgs = do
  let (filename:args) = outerArgs
  files <- getFiles
  let needle = fromString filename
  let matches = fmap (take 1 . mapCompareExpand files) needle
  case matches of Nothing -> return ()
                  (Just []) -> putStrLn "not found"
                  (Just l) -> mapM_ (\sr -> callProcess (showLong sr) args) l
  
