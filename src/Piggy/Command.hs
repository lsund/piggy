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

fromLine :: [String] -> (String, Command)
fromLine [a, b, c] = (a, Command b (read c))
