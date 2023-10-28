module Expr
  ( Expr(..),
    Object(..)
  ) where

import Token

data Expr
  = Assign Token Expr
  | Binary Expr Token Expr
  | Call Expr Token [Expr]
  | Get Expr Token
  | Grouping Expr
  | Literal Object
  | Logical Expr Token Expr
  | Unary Token Expr
  | Variable Token
  deriving (Show, Eq)

data Object
  = Number Double
  | String String
  | Bool Bool
  | Undefined
  deriving (Show, Eq)


