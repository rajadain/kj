{-# LANGUAGE OverloadedStrings #-}

module Dir (mapCompareExpand,
            ScriptFileData,
            showLong,
            showShort,
            fromString,
            getFiles,
            fileName) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe

------------------------------------------------------------
-- types
------------------------------------------------------------

-- the purpose of a scriptfile is to encapsulate the magic feature
-- of resolving script names without extensions.
-- it should be possible to :
--    compare a less complete file to a more complete file and get
--    the more complete.
data ScriptFileData = ScriptFileData { fileName :: String,
                                       pathTo :: Maybe FilePath,
                                       extension :: Maybe String } deriving Show

data KjConfig = KjConfig { kjDir :: String }

instance FromJSON KjConfig where
  parseJSON (Object o) = KjConfig <$> (o .: "kjDir")

instance Eq ScriptFileData where
  sr1 == sr2 = isJust $ compareExpand sr1 sr2

mapCompareExpand :: [ScriptFileData] -> ScriptFileData -> [ScriptFileData]
mapCompareExpand haystack needle = take 1 $ mapMaybe (compareExpand needle) haystack

-- given two script files, if they are equal, return as much
-- information as possible from both. implicitly assumes it will
-- never need to resolve *conflicting* information.
compareExpand :: ScriptFileData -> ScriptFileData -> Maybe ScriptFileData
compareExpand (ScriptFileData f1 p1 e1) (ScriptFileData f2 p2 e2) =
  if f1 /= f2
  then Nothing
  else Just $ ScriptFileData f1 (p1 `mplus` p2) (e1 `mplus` e2)

showLong :: ScriptFileData -> String
showLong sr = prefixFn $ fileName sr ++ rest
  where prefixFn = case pathTo sr of Nothing -> id
                                     Just p -> \sr' -> joinPath [p, sr']
        rest = case extension sr of Nothing -> ""
                                    Just e -> "." ++ e

showShort :: ScriptFileData -> String
showShort = fileName

fromString :: String -> Maybe ScriptFileData
fromString s = case chunks of (f, "") -> Just $ ScriptFileData f dir Nothing
                              (f, ".py") -> Just $ ScriptFileData f dir (Just "py")
                              (f, ".sh") -> Just $ ScriptFileData f dir (Just "sh")
                              _ -> Nothing
  -- TODO: hidden dotfiles are not gonna parse correctly here:
  where chunks = span (/='.') filePart
        filePart = takeFileName s
        dirPart = takeDirectory s
        dir = case dirPart of "." -> Nothing
                              x -> Just x

------------------------------------------------------------
-- public api
------------------------------------------------------------

getFiles :: IO [ScriptFileData]
getFiles = getCurrentDirectory >>= getAllPossibleKjDirs >>= getAllFiles

------------------------------------------------------------
-- directory IO
------------------------------------------------------------

listAndJoin :: FilePath -> IO [FilePath]
listAndJoin path = fmap (map (\f -> joinPath [path, f])) (listDirectory path)

getAllFiles :: [FilePath] -> IO [ScriptFileData]
getAllFiles kjDirs = fmap concat $ mapM getAllFiles' kjDirs
  where getAllFiles' kjDir = do
          exists <- doesDirectoryExist kjDir
          if not exists
            then return []
            else do
            fullPaths <- listAndJoin kjDir
            filesOnly <- filterM doesFileExist fullPaths
            dirsOnly <- filterM doesDirectoryExist fullPaths
            nestedFiles <- concat `fmap` mapM getAllFiles' dirsOnly
            return $ (mapMaybe fromString) filesOnly ++ nestedFiles

getAllPossibleKjDirs :: FilePath -> IO [FilePath]
getAllPossibleKjDirs path = mapM getKjDir dirs
  where dirs = getAllParents path

-- see if the folder has a .kj.json
-- try to parse kj.json and get a kj dir from it
-- if it fails for any reason, , default to "scripts"
-- return the full path to the kj dir or "scripts"
getKjDir :: FilePath -> IO FilePath
getKjDir path = do
  let join = \j -> joinPath [path, j]
  let defaultPath = join "scripts"
  let configPath = join ".kj.json"
  hasConfig <- doesFileExist $ configPath
  if not hasConfig
    then return defaultPath
    else do
    parsed <- fmap (decode . BL.pack) $ readFile configPath
    return $ case parsed of Nothing -> defaultPath
                            (Just v) -> join (kjDir v)

------------------------------------------------------------
-- directory content transformations
------------------------------------------------------------

-- split up a file path into its components and
-- rejoin its components in every combination that
-- produces a valid parent path.
getAllParents :: FilePath -> [FilePath]
getAllParents path = reverse $ map ($ chunks) takers
  where takeAndConcat n = joinPath . take n
        chunks = splitPath path
        takers = map takeAndConcat [1 .. length chunks]
