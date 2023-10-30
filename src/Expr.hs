module Expr
  ( Expr(..),
    Object(..)
  ) where

import Token
import Object

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
