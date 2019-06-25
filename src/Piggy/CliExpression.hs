module Piggy.CliExpression where

import Data.List (intercalate, isInfixOf)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Piggy.Util
import Prelude
import Control.Lens

class CliExpression a where
  expand :: a -> String

match :: CliExpression a => String -> Map String a -> Maybe FilePath
match x =
  listToMaybe .
  revSort length .
  map (expand . snd) . M.toList . M.filterWithKey (const . isInfixOf x)

columnWidth :: Int
columnWidth = 75

format :: (CliExpression a, Ord a) => Map String a -> String
format m =
  let xs = sort (^. _2) $ M.toList m
   in intercalate "\n" $
      map (\(x, y) -> expand y <> makeSpace (expand y) <> x) xs
  where
    makeSpace x = replicate (columnWidth - length x) ' '
