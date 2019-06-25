{-# LANGUAGE OverloadedStrings #-}

module Main where

import Piggy.Command as Command
import Piggy.Location as Location
import Piggy.CliExpression as CliExpression

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

cmdSpecFile :: FilePath -> FilePath
cmdSpecFile = flip (<>) "/cmds.csv"
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

firstMatch :: CliExpression a => String -> String -> Map String a -> String
firstMatch fallback tag = fromMaybe fallback . CliExpression.match tag

handleCommand :: (Map String Location, Map String Command) -> [String] -> IO String
handleCommand (locs, _) ("cd":tag:_) = return $ firstMatch "." tag locs
handleCommand _ ("cd":_) = return ""
handleCommand (locs, _) ("cdl":_) = (return . CliExpression.format) locs
handleCommand (_, cmds) ("rl":_) = (return . CliExpression.format) cmds
handleCommand params ["ad", path] = do
  basedir <- last . splitOn "/" <$> expandPath path
  handleCommand params ["ad", path, basedir]
handleCommand (locs, cmds) ("ad":path:tag:_) =
  addDirTo (dirSpecFile resourcesDir) tag path >>=
  (\(x, loc) -> handleCommand (M.insert x loc locs, cmds) ["cdl"])
handleCommand (_, cmds) ("r":tag:_) = return $ firstMatch ";" tag cmds
handleCommand _ _ = return "Unknown command"

readSpecFrom :: ([String] -> (String, a)) -> FilePath -> IO (Map String a)
readSpecFrom parseLine fname =
  M.fromList <$>
  ((mapM (return . parseLine . splitOn ",") . lines) =<< readFile fname)

touchIfNotExists :: FilePath -> IO ()
touchIfNotExists path =
  doesFileExist path >>= (\exists -> unless exists $ writeFile path "")

main :: IO ()
main = do
  createDirectoryIfMissing True resourcesDir
  let dirSpec = dirSpecFile resourcesDir
      cmdSpec = cmdSpecFile resourcesDir
  touchIfNotExists dirSpec
  touchIfNotExists cmdSpec
  dirs <- readSpecFrom Location.fromLine dirSpec
  cmds <- readSpecFrom Command.fromLine cmdSpec
  getArgs >>= handleCommand (dirs, cmds) >>= putStrLn
