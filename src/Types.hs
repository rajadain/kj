module Types where

import Options


--- this type is just used for parsing command line options
--- they are processed into an `App`, which gets passed around
--- to lower-level functions.
data KjOptions = KjOptions { optListOnly :: Bool
                           , optDetailOnly :: Bool
                           , optCatFile :: Bool
                           , optAutoRestart :: Bool
                           , optVerbose :: Bool
                           }

instance Options KjOptions where
    defineOptions =
      pure KjOptions
      <*> mkViewOpt "list" "Print all available scripts (in machine readable format)"
      <*> mkViewOpt "detail" "Print all available scripts (with docstring if available)"
      <*> mkViewOpt "cat" "display contents of script"
      <*> mkViewOpt "auto-restart" "automatically restart script when it terminates"
      <*> mkViewOpt "verbose" "run in verbose mode"
      where mkViewOpt long desc =
              defineOption optionType_bool
              (\o -> o { optionLongFlags = [long],
                         optionShortFlags = [head long],
                         optionDescription = desc,
                         optionDefault = False })

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
