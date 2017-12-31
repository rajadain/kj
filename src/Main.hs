{-# LANGUAGE FlexibleContexts #-}
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

  let appRunMode = case _app_runMode app of
          RunMode_List -> listOnlyMode
          RunMode_Detail -> detailOnlyMode
          RunMode_Cat -> catFileMode
          RunMode_Execute -> scriptMode

  runReaderT appRunMode app

listOnlyMode :: (MonadReader App m, MonadIO m) => m ()
listOnlyMode = do
  f <- getFiles
  mapM_ (liftIO . putStrLn . showShort) f

detailOnlyMode :: (MonadReader App m, MonadIO m) => m ()
detailOnlyMode = do
  files <- getFiles
  texts <- liftIO $ mapM (readFile . showLong) files
  let parsed = zipWith parseScript (map fileName files) texts
  liftIO $ mapM_ print (catMaybes parsed)

withFileMode :: (MonadReader App m, MonadIO m) => (String -> [String] -> IO ()) -> m ()
withFileMode f = do
  (App _ _ _ args) <- ask
  case args of
    [] -> liftIO $ putStrLn $ parsedHelp (parseOptions args::ParsedOptions KjOptions)
    (filename:scriptArgs) -> do
      files <- getFiles
      let needle = fromString filename
      let matches = fmap (take 1 . mapCompareExpand files) needle
      case matches of
        Nothing -> return ()
        (Just []) -> liftIO $ putStrLn "not found"
        (Just l) -> liftIO $ f (showLong . head $ l) scriptArgs

catFileMode :: (MonadReader App m, MonadIO m) => m ()
catFileMode = withFileMode f
  where f p _ = putStr =<< readFile p

scriptMode :: (MonadReader App m, MonadIO m) => m ()
scriptMode = do
  (App _ autoRestart _ _) <- ask
  withFileMode (\f args -> repeatProcess autoRestart f args)

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
