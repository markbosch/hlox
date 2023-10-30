module Token
  ( Token(..)
  ) where

import TokenType
import Object (Object)

data Token = Token
  { token_type :: TokenType,
    lexeme :: String,
    literal :: Maybe Object,
    line :: Int
  } deriving (Show, Eq)
