module Piggy.Location where

import Piggy.Util
import Prelude

import Control.Lens
import Data.List (intercalate, isInfixOf)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)

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

fromLine :: [String] -> (String, Location)
fromLine [a, b, c] = (a, Location b (read c))
fromLine _ = undefined

format :: Map String Location -> String
format m =
  let xs = sort (^. _2) $ M.toList m
   in intercalate "\n" $ map (\(x, y) -> _path y <> makeSpace (_path y) <> x) xs
  where
    makeSpace x = replicate (columnWidth - length x) ' '

match :: String -> Map String Location -> Maybe FilePath
match x =
  listToMaybe .
  revSort length .
  map (_path . snd) . M.toList . M.filterWithKey (const . isInfixOf x)
