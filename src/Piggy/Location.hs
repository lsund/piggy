module Piggy.Location where

import Prelude
import Piggy.Util

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.List (intercalate)

data Location =
  Location
    { _path :: FilePath
    , _timesAccessed :: Int
    }
  deriving (Eq, Show)

instance Ord Location where
  compare (Location _ x) (Location _ y) = x `compare` y

columnWidth :: Int
columnWidth = 75

parseLine :: [String] -> (String, Location)
parseLine [a, b, c] = (a, Location b (read c))
parseLine _ = undefined

fmtDirs :: Map String Location -> String
fmtDirs m =
  let xs = sort (^. _2) $ M.toList m
   in intercalate "\n" $
      map (\(x, y) -> _path y <> makeSpace (_path y) <> x) xs
  where
    makeSpace x = replicate (columnWidth - length x) ' '

matchDir :: Map String Location -> String -> Maybe FilePath
matchDir m x =
  let matches = M.filterWithKey (const . match x) m
   in listToMaybe $ revSort length $ map (_path . snd) $ M.toList matches
