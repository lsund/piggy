module Piggy.Util where

import Prelude
import Data.List (isInfixOf, sortBy)

match :: String -> String -> Bool
match = isInfixOf

sort :: Ord b => (a -> b) -> [a] -> [a]
sort f = sortBy (\x y -> f x `compare` f y)

revSort :: Ord b => (a -> b) -> [a] -> [a]
revSort f = sortBy (\x y -> f y `compare` f x)
