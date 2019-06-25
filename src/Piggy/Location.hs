module Piggy.Location where

import           Piggy.CliExpression
import           Prelude

data Location =
  Location
    { _path          :: FilePath
    , _timesAccessed :: Int
    }
  deriving (Eq, Show)

instance Ord Location where
  compare (Location _ x) (Location _ y) = x `compare` y

instance CliExpression Location where
  expand = _path

fromLine :: [String] -> (String, Location)
fromLine [a, b, c] = (a, Location b (read c))
fromLine _         = undefined
