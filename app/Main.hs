{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import qualified Data.List as L
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Prelude
import System.Directory
import System.Environment

data Location =
  Location
    { _path :: FilePath
    , _timesAccessed :: Int
    }
  deriving (Eq, Show)

instance Ord Location where
  compare (Location _ x) (Location _ y) = x `compare` y

--------------------------------------------------------------------------------
-- Configuration
resourcesDir :: FilePath
resourcesDir = "/home/lsund/.piggy/resources"

--------------------------------------------------------------------------------
-- Code

dirSpecFile :: FilePath -> FilePath
dirSpecFile = flip (<>) "/dirs.csv"

columnWidth :: Int
columnWidth = 75

parseLine :: [String] -> (String, Location)
parseLine [a, b, c] = (a, Location b (read c))
parseLine _ = undefined

sort :: Ord b => (a -> b) -> [a] -> [a]
sort f = sortBy (\x y -> f x `compare` f y)

revSort :: Ord b => (a -> b) -> [a] -> [a]
revSort f = sortBy (\x y -> f y `compare` f x)

fmtDirs :: Map String Location -> String
fmtDirs m =
  let xs = sort (^. _2) $ M.toList m
   in L.intercalate "\n" $
      map (\(x, y) -> _path y <> makeSpace (_path y) <> x) xs
  where
    makeSpace x = replicate (columnWidth - length x) ' '

match :: String -> String -> Bool
match = L.isInfixOf

matchDir :: Map String Location -> String -> Maybe FilePath
matchDir m x =
  let matches = M.filterWithKey (const . match x) m
   in listToMaybe $ revSort length $ map (_path . snd) $ M.toList matches

--------------------------------------------------------------------------------
-- IO

expandPath :: FilePath -> IO FilePath
expandPath path =
  if path == "."
    then getCurrentDirectory
    else pure path

addDirTo :: FilePath -> String -> FilePath -> IO (String, Location)
addDirTo fname tag path = do
  expandedPath <- expandPath path
  (tag, Location expandedPath 0) <$
    appendFile fname (tag <> "," <> expandedPath <> ",0\n")

handleCommand :: Map String Location -> [String] -> IO String
handleCommand m ("cd":x:_) = return $ fromMaybe "." (matchDir m x)
handleCommand _ ("cd":_) = return ""
handleCommand m ("cdl":_) = return . fmtDirs $ m
handleCommand m ["a", path] = do
  basedir <- last . splitOn "/" <$> expandPath path
  handleCommand m ["a", path, basedir]
handleCommand m ("a":path:tag:_) =
  addDirTo (dirSpecFile resourcesDir) tag path >>=
  (\(x, loc) -> handleCommand (M.insert x loc m) ["cdl"])
handleCommand _ _ = return "Unknown command"

readDirsFrom :: FilePath -> IO (Map String Location)
readDirsFrom fname =
  M.fromList <$>
  ((mapM (return . parseLine . splitOn ",") . lines) =<< readFile fname)

main :: IO ()
main = do
  let dirSpec = dirSpecFile resourcesDir
  createDirectoryIfMissing True resourcesDir
  dirSpecExists <- doesFileExist dirSpec
  unless dirSpecExists $ writeFile dirSpec ""
  dirs <- readDirsFrom dirSpec
  getArgs >>= handleCommand dirs >>= putStrLn
