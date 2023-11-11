module RecursiveParser
  ( expression
  ) where

-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" ;

import Expr
import Token
import TokenType

newtype ParseError = ParseError String deriving (Show)

makeBinary :: Expr -> Token -> [Token] -> ([Token] -> (Expr, [Token])) -> (Expr, [Token])
makeBinary expr token tokens f = (\(expr',tokens') -> (Binary expr token expr', tokens')) $ f tokens

-- The parser
expression :: [Token] -> Either ParseError Expr
expression tokens = do
  case equality tokens of
    (expr, []) -> Right expr
    (_, remaining) -> Left $ ParseError $ "Parsing error. Remaining tokens: " ++ show remaining

-- Equality
equality :: [Token] -> (Expr, [Token])
equality tokens = case comparison tokens of
  (expr, Token BANG_EQUAL _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ comparison
  (expr, Token EQUAL_EQUAL _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ comparison
  (expr, tokens') -> (expr, tokens')


-- comparison
comparison :: [Token] -> (Expr, [Token])
comparison tokens = case term tokens of
  (expr, Token GREATER _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ term
  (expr, Token GREATER_EQUAL _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ term
  (expr, Token LESS _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ term
  (expr, Token LESS_EQUAL _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ term
  (expr, tokens') -> (expr, tokens')

-- term
term :: [Token] -> (Expr, [Token])
term tokens = case factor tokens of
  (expr, Token PLUS _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ factor
  (expr, Token MINUS _ _ _: tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ factor
  (expr, tokens') -> (expr, tokens')

-- factor
factor :: [Token] -> (Expr, [Token])
factor tokens = case unary tokens of
  (expr, Token STAR _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ unary
  (expr, Token SLASH _ _ _ : tokens') ->
    makeBinary expr (tokens !! 1) tokens' $ unary
  (expr, tokens') -> (expr, tokens')

-- unary
unary :: [Token] -> (Expr, [Token])
unary tokens =
  case tokens of
    (Token BANG _ _ _ : tokens') ->
      makeUnary (head tokens) tokens'
    (Token MINUS _ _ _: tokens') ->
      makeUnary (head tokens) tokens'
    _ -> primary tokens
  where
    makeUnary :: Token -> [Token] -> (Expr, [Token])
    makeUnary token tokens =
      (Unary token expr, tokens')
      where
        (expr, tokens') = unary tokens

-- primary
primary :: [Token] -> (Expr, [Token])
primary tokens =
  case tokens of
    (Token NUMBER lex _ _ : rest) -> (Literal (Number (read lex)), rest)
    (Token STRING lex _ _ : rest) -> (Literal (String lex), rest)
    (Token TRUE _ _ _ : rest) -> (Literal (Bool True), rest)
    (Token FALSE _ _ _ : rest) -> (Literal (Bool False), rest)
    (Token NIL _ _ _ : rest) -> (Literal Nil, rest)
    (Token IDENTIFIER lex _ _ : rest) -> (Variable (head tokens), rest)
    _ -> error "Unexpected token in primary expression"
