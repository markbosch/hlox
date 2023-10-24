module Token
  ( Token(..)
  ) where

import TokenType

data Token = Token
  { token_type :: TokenType,
    lexeme :: String,
    literal :: Maybe Literal,
    line :: Int
  } deriving (Show, Eq, Ord)
