module Types where

-- this type is just used for parsing command line options
-- they are processed into an `App`, which gets passed around
-- to lower-level functions.
data KjOptions = KjOptions { optListOnly :: Bool
                           , optDetailOnly :: Bool
                           , optCatFile :: Bool
                           , optAutoRestart :: Bool
                           , optVerbose :: Bool
                           }

data RunMode = RunMode_List
             | RunMode_Detail
             | RunMode_Cat
             | RunMode_Execute

data App = App
  { _app_runMode :: RunMode
  , _app_autoRestart :: Bool
  , _app_verbose :: Bool
  , _app_args :: [String]
  }


