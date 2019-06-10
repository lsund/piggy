module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude
import System.Directory
import System.Environment

dirSpecFile :: FilePath
dirSpecFile = "/home/lsund/Documents/tech/repos/piggy/resources/dirs.csv"

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = undefined

match :: String -> String -> Bool
match = L.isInfixOf

fmtDirs :: Map String String -> String
fmtDirs = L.intercalate "\n" . M.elems

addDirTo :: FilePath -> String -> String -> IO String
addDirTo fname tag path = do
  cwd <- getCurrentDirectory
  "OK" <$
    appendFile
      fname
      ("\n" <> tag <> "," <>
       if path == "."
         then cwd
         else path)

handleCommand :: Map String String -> [String] -> IO String
handleCommand dirmap ("cd":x:_) =
  return . fmtDirs $ M.filterWithKey (const . match x) dirmap
handleCommand dirmap ("cdl":_) = return . fmtDirs $ dirmap
handleCommand _ ("a":tag:path:_) = addDirTo dirSpecFile tag path
handleCommand _ _ = undefined

readDirsFrom :: FilePath -> IO (Map String String)
readDirsFrom fname =
  M.fromList <$>
  ((mapM (return . tuplify2 . splitOn ",") . lines) =<< readFile fname)

main :: IO ()
main = do
  createDirectoryIfMissing True "resources"
  dirs <- readDirsFrom dirSpecFile
  getArgs >>= handleCommand dirs >>= putStrLn
