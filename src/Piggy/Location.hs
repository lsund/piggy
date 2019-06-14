module Piggy.Location where

import Piggy.Util
import Piggy.CliExpression
import Prelude

import Control.Lens
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map.Strict as M

data Location =
  Location
    { _path :: FilePath
    , _timesAccessed :: Int
    }
  deriving (Eq, Show)

instance Ord Location where
  compare (Location _ x) (Location _ y) = x `compare` y

instance CliExpression Location where
  expand = _path

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
