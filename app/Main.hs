module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory
import System.Environment
import System.Process

dirSpecFile = "/home/lsund/Documents/tech/repos/piggy/resources/dirs.csv"

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = undefined

match :: String -> String -> Bool
match = L.isInfixOf

fmtDirs :: Map String String -> String
fmtDirs = concat . L.intersperse "\n" . M.elems

addDirTo :: FilePath -> String -> String -> IO String
addDirTo fname tag path = do
  cwd <- getCurrentDirectory
  return "OK" <$>
    (appendFile fname $
     "\n" <> tag <> "," <>
     if path == "."
       then cwd
       else path)

handleCommand :: Map String String -> [String] -> IO String
handleCommand dirmap ("cd":x:_) =
  return . fmtDirs $ M.filterWithKey (const . (match x)) dirmap
handleCommand dirmap ("cdl":_) = return . fmtDirs $ dirmap
handleCommand dirmap ("a":tag:path:_) = addDirTo dirSpecFile tag path
handleCommand _ _ = undefined

readDirsFrom :: FilePath -> IO (Map String String)
readDirsFrom fname =
  M.fromList <$>
  ((mapM (return . tuplify2 . splitOn ",") . lines) =<< readFile fname)

main :: IO ()
main = do
  dirs <- readDirsFrom dirSpecFile
  s <- (handleCommand dirs) =<< getArgs
  putStrLn s
