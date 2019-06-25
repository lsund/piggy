module Piggy.Command where

import Piggy.CliExpression
import Prelude

data Command =
  Command
    { _command :: String
    , _timesExecuted :: Int
    }
  deriving (Eq, Show)

instance CliExpression Command where
  expand = _command

instance Ord Command where
  compare (Command _ x) (Command _ y) = x `compare` y

fromLine :: [String] -> (String, Command)
fromLine [a, b, c] = (a, Command b (read c))
fromLine _ = undefined
