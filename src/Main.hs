{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Options
import Data.Maybe
import System.Process hiding (runCommand)
import Text.Printf

import Dir
import Parse
import Types
import System.Exit

import Control.Monad.Trans
import Control.Monad.Reader


main :: IO ()
main = do
  app <- runCommand $ \opts args -> do
    let runMode
          | optDetailOnly opts = RunMode_Detail
          | optListOnly opts = RunMode_List
          | optCatFile opts = RunMode_Cat
          | otherwise = RunMode_Execute

    return App { _app_runMode = runMode
               , _app_verbose = optVerbose opts
               , _app_autoRestart = optAutoRestart opts
               , _app_args = args
               }

  case _app_runMode app of
    RunMode_List -> listOnlyMode app
    RunMode_Detail -> detailOnlyMode app
    RunMode_Cat -> catFileMode app
    RunMode_Execute -> scriptMode app

listOnlyMode :: App -> IO ()
listOnlyMode app = do
  f <- runReaderT getFiles app
  mapM_ (putStrLn . showShort) f

detailOnlyMode :: App -> IO ()
detailOnlyMode app = (flip runReaderT) app $ do
  files <- getFiles
  texts <- liftIO $ mapM (readFile . showLong) files
  let parsed = zipWith parseScript (map fileName files) texts
  liftIO $ mapM_ print (catMaybes parsed)

withFileMode :: (String -> [String] -> IO ()) -> App -> IO ()
withFileMode f app@(App _ _ _ args) =
  case args of
  [] -> putStrLn $ parsedHelp (parseOptions args::ParsedOptions KjOptions)
  (filename:scriptArgs) -> do
    files <- runReaderT getFiles app
    let needle = fromString filename
    let matches = fmap (take 1 . mapCompareExpand files) needle
    case matches of
      Nothing -> return ()
      (Just []) -> putStrLn "not found"
      (Just l) -> f (showLong . head $ l) scriptArgs

catFileMode :: App -> IO ()
catFileMode app = withFileMode f app
  where f p _ = putStr =<< readFile p

scriptMode :: App -> IO ()
scriptMode app = withFileMode (\f args' -> repeatProcess (_app_autoRestart app) f args') app

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
