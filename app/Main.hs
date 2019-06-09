module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment
import System.Process

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = undefined

match :: String -> String -> Bool
match = L.isInfixOf

fmtDirs :: Map String String -> String
fmtDirs = concat . L.intersperse "\n" . M.elems

handleCommand :: Map String String -> [String] -> String
handleCommand dirmap ("cd":x:_) = fmtDirs $ M.filterWithKey (const . (match x)) dirmap
handleCommand dirmap ("cdl":_) = fmtDirs $ dirmap
handleCommand _ _ = undefined

readDirsFrom :: String -> IO (Map String String)
readDirsFrom path =
  M.fromList <$>
  ((mapM (return . tuplify2 . splitOn ",") . lines) =<< readFile path)

main :: IO ()
main = do
  dirs <- readDirsFrom "resources/dirs.csv"
  (putStrLn . handleCommand dirs) =<< getArgs
