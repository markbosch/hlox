-- Parser
-- https://craftinginterpreters.com/parsing-expressions.html

-- CFG - Context Free Grammer
-- expression  -> equality
-- equality    -> comparison ( ( "!=" | "==" ) comparison )*
-- comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )*
-- term        -> factor ( ( "-" | "+" ) factor )*
-- factor      -> unary ( ( "/" | "*" ) unary )*
-- unary       -> ( "!" | "-" ) unary | primary
-- primary     -> NUMBER | STRING | "true" | "false" | "nil"
--              | "(" expression ")"

module Parser
  (
    parse,
    tokenParser,
    primary,
    equality,
    unary,
    factor,
    term,
    expression,
    combine,
    comparison
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
                             (v,out):_ -> [(f v, out)])
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
match tokenTypes = do
    token <- tokenParser
    if token_type token `elem` tokenTypes
        then return token
        else empty


expression :: Parser Expr
expression = equality

equality :: Parser Expr
equality = do
    left <- combine
    token <- match [BANG_EQUAL, EQUAL_EQUAL]
    Binary left token <$> combine

comparison :: Parser Expr
comparison = do
    left <- combine
    token <- match [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]
    Binary left token <$> combine

combine :: Parser Expr
combine = unary <|> comparison <|> factor <|> term

term :: Parser Expr
term = do
    left <- combine
    token <- match [MINUS, PLUS]
    Binary left token <$> combine

-- factor      -> unary ( ( "/" | "*" ) unary )*
factor :: Parser Expr
factor = do
    left <- unary
    token <- match [SLASH, STAR]
    right <- unary
    return $ Binary left token right

unary :: Parser Expr
unary = primary <|> Unary <$> match [BANG, MINUS] <*> unary

primary :: Parser Expr
primary = do
    token <- tokenParser
    case token_type token of
        FALSE -> return $ Literal (Bool False)
        TRUE -> return $ Literal (Bool True)
        NIL -> return $ Literal Nil
        NUMBER -> return $ Literal (Expr.Number (read (lexeme token)))
        STRING -> return $ Literal (Expr.String (lexeme token))
        LEFT_PAREN -> do
            expr <- expression
            match [RIGHT_PAREN]
            return expr
        _ -> empty  -- Return empty if none of the expected primary expressions match
