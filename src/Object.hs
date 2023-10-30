module Object
  ( Object(..)
  ) where

data Object
  = Number Double
  | String String
  | Bool Bool
  | Nil
  deriving (Show, Eq)