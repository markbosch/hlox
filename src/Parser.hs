-- Parser
-- https://craftinginterpreters.com/parsing-expressions.html

module Parser
  (
    parse,
    tokenParser,
    primary,
    equality,
    unary,
    multiplication
  ) where

import Control.Applicative

import Expr
import Token
import TokenType

-- Parser
newtype Parser a = P ([Token] -> [(a,[Token])])

-- Functor for Parser
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\tokens -> case parse p tokens of
                             []        -> []
                             [(v,out)] -> [(f v, out)])

-- Applicative for Parser
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\tokens -> [(v,tokens)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\tokens -> case parse pg tokens of
                              []        -> []
                              [(g,out)] -> parse (fmap g px) out)

-- Monad for Parser
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\tokens -> case parse p tokens of
                            []        -> []
                            [(v,out)] -> parse (f v) out)

-- Alternative for Parser
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\tokens -> [])
  -- (<|>)
  p <|> q = P (\tokens -> case parse p tokens of
                             []        -> parse q tokens
                             [(v,out)] -> [(v,out)])

parse :: Parser a -> [Token] -> [(a,[Token])]
parse (P p) tokens = p tokens

tokenParser :: Parser Token
tokenParser = P $ \tokens -> case tokens of
                               []     -> []
                               (t:ts) -> [(t, ts)]

-- match based on a list of tokentypes and return the first match
match :: [TokenType] -> Parser Token
match [] = empty -- No TokenTypes to match
match (t:ts) = do
    token <- tokenParser
    if token_type token == t
        then return token
        else match ts

equality :: Parser Expr
equality = do
    left <- comparison
    token <- match [TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL]
    right <- comparison
    return $ Binary left token right

comparison :: Parser Expr
comparison = do
    left <- addition
    token <- match [TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL]
    right <- addition
    return $ Binary left token right

addition :: Parser Expr
addition = do
    left <- multiplication
    token <- match [TokenType.MINUS, TokenType.PLUS]
    right <- multiplication
    return $ Binary left token right

multiplication :: Parser Expr
multiplication = do
    left <- unary
    token <- match [TokenType.SLASH, TokenType.STAR]
    right <- unary
    return $ Binary left token right

unary :: Parser Expr  
unary = do
    token <- match [TokenType.BANG, TokenType.MINUS]
    right <- primary
    return $ Unary token right

primary :: Parser Expr
primary = do
    token <- tokenParser
    case token_type token of
        TokenType.FALSE -> return $ Literal (Bool False)
        TokenType.TRUE -> return $ Literal (Bool True)
        TokenType.NIL -> return $ Literal Nil
        TokenType.NUMBER -> return $ Literal (Expr.Number (read (lexeme token)))
        TokenType.STRING -> return $ Literal (Expr.String (lexeme token))
        TokenType.LEFT_PAREN -> do
            expr <- expression
            match [TokenType.RIGHT_PAREN]
            return expr
        _ -> empty  -- Return empty if none of the expected primary expressions match

expression :: Parser Expr
expression = equality    

-- match (?!?!) with predicate
-- if tokentype == 

-- parse tokenParser [(Token.Token TokenType.MINUS "-" Nothing 1)]



--parsers :: [Parser Expr]
--parsers =
--  [
--    equality
--  ]

--parseTokens :: [Token] -> Expr
--parse primary [(Token.Token TokenType.TRUE "" (Just $ Bool True) 1)] 
