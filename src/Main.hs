{-# LANGUAGE OverloadedStrings #-}

module Main where

import Piggy.CliExpression as CliExpression
import Piggy.Command as Command
import Piggy.Location as Location

import Control.Monad
import Data.List (intercalate)
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
resourcesDir = "/home/lsund/.piggy/runtime-resources"

delimiter :: String
delimiter = "#"

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
    appendFile fname (tag <> delimiter <> expandedPath <> delimiter <> "0\n")

addCommandTo :: FilePath -> String -> String -> IO (String, Command)
addCommandTo fname tag command =
  (tag, Command command 0) <$
  appendFile fname (tag <> delimiter <> command <> delimiter <> "0\n")

firstMatch :: CliExpression a => String -> String -> Map String a -> String
firstMatch fallback tag = fromMaybe fallback . CliExpression.match tag

formatHelp :: Map String String -> String
formatHelp m =
  intercalate "\n" $ map (\(x, y) -> x <> makeSpace x <> y) $ M.toList m
  where
    makeSpace x = replicate (20 - length x) ' '

help :: String
help =
  "Piggy supports the following commands:\n\n" <>
  (formatHelp . M.fromList)
    [ ("h", "Show this help")
    , ("help", "Show this help")
    , ("ar", "Add command")
    , ("ad", "Add directory")
    , ("dl", "List directories")
    , ("dlt", "List directory tags")
    , ("rl", "List commands")
    , ("rlt", "List command tags")
    , ("cd", "Enter directory")
    , ("r", "Run command")
    ]

formatTags :: Map String a -> String
formatTags = intercalate "\n" . M.keys

handleCommand ::
     (Map String Location, Map String Command) -> [String] -> IO String
-- Help
handleCommand _ [] = return help
handleCommand _ ("h":_) = return help
handleCommand _ ("help":_) = return help
-- Change directory
handleCommand (locs, _) ("cd":tag:_) = return $ firstMatch "." tag locs
handleCommand _ ("cd":_) = return ""
-- Add a directory
handleCommand params ["ad"] =
  expandPath "." >>= (\path -> handleCommand params ["ad", path])
handleCommand params ["ad", path] =
  last . splitOn "/" <$> expandPath path >>=
  (\basedir -> handleCommand params ["ad", path, basedir])
handleCommand (locs, cmds) ("ad":path:tag:_) =
  addDirTo (dirSpecFile resourcesDir) tag path >>=
  (\(x, loc) -> handleCommand (M.insert x loc locs, cmds) ["dl"])
-- Run command
handleCommand (_, cmds) ("r":tag:_) = return $ firstMatch ";" tag cmds
-- Add command
handleCommand (locs, cmds) ("ar":command:tag:_) =
  addCommandTo (cmdSpecFile resourcesDir) tag command >>=
  (\(x, cmd) -> handleCommand (locs, M.insert x cmd cmds) ["rl"])
-- Lists
handleCommand (locs, _) ("dl":_) = (return . CliExpression.format) locs
handleCommand (_, cmds) ("rl":_) = (return . CliExpression.format) cmds
handleCommand (locs, _) ("dlt":_) = (return . formatTags) locs
handleCommand (_, cmds) ("rlt":_) = (return . formatTags) cmds
handleCommand _ _ = return "Unknown command"

readSpecFrom :: ([String] -> (String, a)) -> FilePath -> IO (Map String a)
readSpecFrom parseLine fname =
  M.fromList <$>
  ((mapM (return . parseLine . splitOn delimiter) . lines) =<< readFile fname)

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
