module Main where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment
import System.Process

dirmap :: Map String String
dirmap =
  M.fromList
    [ ("scripts", "/home/lsund/Documents/scripts")
    , ("dotfiles", "/home/lsund/Documents/dotfiles")
    , ("finances", "/home/lsund/Documents/tech/repos/finances")
    , ("trainer", "/home/lsund/Documents/tech/repos/trainer")
    , ("repos", "/home/lsund/Documents/tech/repos")
    , ("goals", "/home/lsund/Documents/tech/repos/goals")
    ]

match :: String -> String -> Bool
match = L.isInfixOf

fmtDirs :: Map String String -> String
fmtDirs = concat . L.intersperse "\n" . M.elems

handleCommand :: [String] -> String
handleCommand ("cd":x:_) = fmtDirs $ M.filterWithKey (const . (match x)) dirmap
handleCommand ("cdl":_) = fmtDirs $ dirmap
handleCommand _ = undefined

main :: IO ()
main = (putStrLn . handleCommand) =<< getArgs
