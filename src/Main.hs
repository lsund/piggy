{-# LANGUAGE OverloadedStrings #-}

module Main where

import Piggy.Location as Location

import Control.Monad
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Prelude
import System.Directory
import System.Environment

--------------------------------------------------------------------------------
-- Configuration
resourcesDir :: FilePath
resourcesDir = "/home/lsund/.piggy/resources"

--------------------------------------------------------------------------------
-- Code
dirSpecFile :: FilePath -> FilePath
dirSpecFile = flip (<>) "/dirs.csv"

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
handleCommand locs ("cd":x:_) = (return . fromMaybe "." . Location.match x) locs
handleCommand _ ("cd":_) = return ""
handleCommand locs ("cdl":_) = (return . Location.format) locs
handleCommand locs ["a", path] = do
  basedir <- last . splitOn "/" <$> expandPath path
  handleCommand locs ["a", path, basedir]
handleCommand locs ("a":path:tag:_) =
  addDirTo (dirSpecFile resourcesDir) tag path >>=
  (\(x, loc) -> handleCommand (M.insert x loc locs) ["cdl"])
handleCommand _ _ = return "Unknown command"

readDirsFrom :: FilePath -> IO (Map String Location)
readDirsFrom fname =
  M.fromList <$>
  ((mapM (return . Location.fromLine . splitOn ",") . lines) =<< readFile fname)

main :: IO ()
main = do
  let dirSpec = dirSpecFile resourcesDir
  createDirectoryIfMissing True resourcesDir
  dirSpecExists <- doesFileExist dirSpec
  unless dirSpecExists $ writeFile dirSpec ""
  dirs <- readDirsFrom dirSpec
  getArgs >>= handleCommand dirs >>= putStrLn
