{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude
import System.Directory
import System.Environment

data Location =
  Location
    { _path :: FilePath
    , _timesAccessed :: Int
    }

dirSpecFile :: FilePath
dirSpecFile = "/home/lsund/Documents/tech/repos/piggy/resources/dirs.csv"

columnWidth :: Int
columnWidth = 70

parseLine :: [String] -> (String, Location)
parseLine [x, y, z] = (x, Location y (read z :: Int))
parseLine _ = undefined

match :: String -> String -> Bool
match = L.isInfixOf

compareBy :: Ord c => (a -> c) -> a -> a -> Ordering
compareBy f x y = f x `compare` f y

fmtDirs :: Map String Location -> String
fmtDirs m =
  let keys = M.keys m
      vals = map _path . L.sortBy (compareBy _timesAccessed) . M.elems $ m
   in L.intercalate "\n" $ zipWith (\x y -> y <> makeSpace y <> x) keys vals
  where
    makeSpace x = replicate (columnWidth - length x) ' '

matchDir :: Map String Location -> String -> FilePath
matchDir m x =
  let matches = M.filterWithKey (const . match x) m
  in case length matches of
    1 -> _path . snd . head $ M.toList matches
    _ -> undefined

addDirTo :: FilePath -> String -> FilePath -> IO String
addDirTo fname tag path = do
  cwd <- getCurrentDirectory
  "OK" <$
    appendFile
      fname
      ("\n" <> tag <> "," <>
       if path == "."
         then cwd
         else path <> ",0")

handleCommand :: Map String Location -> [String] -> IO FilePath
handleCommand dirmap ("cd":x:_) = return $ matchDir dirmap x
handleCommand dirmap ("cdl":_) = return . fmtDirs $ dirmap
handleCommand _ ("a":tag:path:_) = addDirTo dirSpecFile tag path
handleCommand _ _ = undefined

readDirsFrom :: FilePath -> IO (Map String Location)
readDirsFrom fname =
  M.fromList <$>
  ((mapM (return . parseLine . splitOn ",") . lines) =<< readFile fname)

main :: IO ()
main = do
  createDirectoryIfMissing True "resources"
  dirs <- readDirsFrom dirSpecFile
  getArgs >>= handleCommand dirs >>= putStrLn
