module Interpreter(
  evaluate,
  isTruthy
) where

import Expr
import Token
import TokenType
import Object

-- Assuming evaluate and visitBinaryExpr are defined
evaluate :: Expr -> Either String Double
evaluate expr = case expr of
  Binary left operator right -> do
    leftValue <- evaluate left
    rightValue <- evaluate right
    case operator of
--      Token GREATER _ _ _ -> return $ leftValue > rightValue
      Token PLUS _ _ _    -> return $ leftValue + rightValue
      Token MINUS _ _ _   -> return $ leftValue - rightValue
      Token SLASH _ _ _   -> if rightValue /= 0 then return $ leftValue / rightValue else Left "Division by zero"
      Token STAR _ _ _    -> return $ leftValue * rightValue
      _ -> Left "Unsupported binary operator"
  Literal object -> case object of
    Number n -> return n
    --String s -> return s
  Unary token expr -> do
    value <- evaluate expr
    case token of
      Token MINUS _ _ _ -> return $ -value

isTruthy :: Object -> Bool
isTruthy Nil = False
isTruthy (Bool False) = False
isTruthy _ = True

--unaryExpr :: Expr -> Int
--unaryExpr (Unary token expr) = do
--  right <- evaluate expr
--  case token of
--    MINUS -> 
