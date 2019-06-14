module Piggy.CliExpression where

import Prelude
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.List (isInfixOf)
import Piggy.Util
import Data.Maybe (listToMaybe)

class CliExpression a where
  expand :: a -> String

match :: CliExpression a => String -> Map String a -> Maybe FilePath
match x =
  listToMaybe .
  revSort length .
  map (expand . snd) . M.toList . M.filterWithKey (const . isInfixOf x)
