-- Scanning
--
-- This is a Haskell Scanner implementation for Lox
-- https://craftinginterpreters.com/scanning.html
--
-- It is based on monadic parser combinators.
-- https://en.wikipedia.org/wiki/Parser_combinator
-- https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
--
-- The Scanner is a parser which will produce
-- tokens as it output

module Scanner
  ( scanTokens
  ) where

import Control.Applicative
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

import Token
import TokenType
import Object

singleCharMapping :: [(TokenType, Char)]
singleCharMapping
  =
  [
    (LEFT_PAREN, '('),  -- Single char tokens
    (RIGHT_PAREN, ')'),
    (LEFT_BRACE, '{'),
    (RIGHT_BRACE, '}'),
    (COMMA, ','),
    (DOT, '.'),
    (MINUS, '-'),
    (PLUS, '+'),
    (SEMICOLON, ';'),
    (SLASH, '/'),
    (STAR, '*'),
    (BANG, '!'),         -- Operators
    (EQUAL, '='),
    (LESS, '<'),
    (GREATER, '>')
  ]

doubleCharMapping :: [(TokenType, String)]
doubleCharMapping
  =
  [
    (EQUAL_EQUAL, "=="),
    (BANG_EQUAL, "!="),
    (EQUAL_EQUAL, "=="),
    (LESS_EQUAL, "<="),
    (GREATER_EQUAL, ">=")
  ]

keywordsMapping :: [(String, TokenType)]
keywordsMapping
  =
  [
    ("and", AND),
    ("class", CLASS),
    ("else", ELSE),
    ("for", FOR),
    ("fun", FUN),
    ("if", IF),
    ("nil", NIL),
    ("or", OR),
    ("print", PRINT),
    ("return", RETURN),
    ("super", SUPER),
    ("this", THIS),
    ("var", VAR),
    ("while", WHILE)
  ]

-- input
type Source = String

-- Scanner
newtype Scanner a = S (Source -> [(a,Source)])

-- Monadic Parsing
instance Functor Scanner where
  -- fmap :: (a -> b) -> Scanner a -> Scanner b
  fmap g s = S (\inp -> case scan s inp of
                           []        -> []
                           [(v,out)] -> [(g v, out)])

instance Applicative Scanner where
  -- pure :: a -> Scanner a
  pure v = S (\inp -> [(v,inp)])
  -- <*> :: Scanner (a -> b) -> Scanner a -> Scanner b
  pg <*> px = S (\inp -> case scan pg inp of
                    []        -> []
                    [(g,out)] -> scan (fmap g px) out)

instance Monad Scanner where
  -- (>>=) :: Scanner a -> (a -> Scanner b) -> Scanner b
  s >>= f = S (\inp -> case scan s inp of
                          []        -> []
                          [(v,out)] -> scan (f v) out)

instance Alternative Scanner where
  -- empty :: Scanner a
  empty = S (\inp -> [])
  -- (<|>) :: Scanner a -> Scanner a -> Scanner a
  s <|> q = S (\inp -> case scan s inp of
                          []        -> scan q inp
                          [(v,out)] -> [(v,out)])

-- Simple building blocks
-- dummy function to get rid of the dummy constructor S
scan :: Scanner a -> Source -> [(a,Source)]
scan (S s) inp = s inp

-- Scan a single char of a input
item :: Scanner Char
item = S (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- return if satisfy predicate
-- else return empty. see Alternative for empty
match :: (Char -> Bool) -> Scanner Char
match s = do x <- item
             if s x then return x else empty

char :: Char -> Scanner Char
char x = match (== x)

string :: String -> Scanner String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

choice :: [Scanner a] -> Scanner a
choice [] = empty
choice (p:ps) = p <|> choice ps

-- whitespace
space :: Scanner Char
space = match isSpace

-- skip whiteSpace
-- this scanner will regonize many whitespace and
-- can be used to ignore it
skipSpace = many space

-- scanToken() function from the Java impl.
-- based on input return a Token
singleCharToken :: Scanner Token
singleCharToken = choice (map build singleCharMapping)
  where
    build :: (TokenType, Char) -> Scanner Token
    build (tokenType, c) = do
      lexeme <- char c <* skipSpace
      return (Token tokenType [c] Nothing 1)

doubleCharToken :: Scanner Token
doubleCharToken = do
  matchingTokens <- choice $ map build doubleCharMapping
  return matchingTokens
  where
    build :: (TokenType, String) -> Scanner Token
    build (tokenType, tokenStr) = do
      lexeme <- string tokenStr <* skipSpace
      return (Token tokenType lexeme Nothing 1)

-- literals
stringLiteral :: Scanner Token
stringLiteral = do
  _ <- char '"' -- Match the opening double quote
  str <- many (match (/= '"')) -- Match characters until a closing double quote
  _ <- char '"' <* skipSpace -- Match the closing double quote
  return (Token STRING str (Just (String str)) 1)
  
numberLiteral :: Scanner Token
numberLiteral = do
  numStr <- some (match isDigit) <* skipSpace -- Match one or more digits
  let numValue = read numStr :: Double -- Parse the matched digits as a Double
  return (Token NUMBER numStr (Just (Number numValue)) 1)

boolLiteral :: Scanner Token
boolLiteral = do
  boolStr <- choice [string "true", string "false"] <* skipSpace
  let token = case boolStr of
                  "true"  -> Token TRUE boolStr (Just (Bool True)) 1
                  "false" -> Token FALSE boolStr (Just (Bool False)) 1
  return token

-- identifier
identifier :: Scanner Token
identifier = do
  firstChar <- match isAlpha  -- Match the first character (must be alphabetical)
  restChars <- many (match isAlphaNum) <* skipSpace  -- Match the remaining characters
  let lexeme = firstChar : restChars
  let tokenType = case findTokenMapping lexeme of
                    Just t -> t  -- It's a reserved word
                    Nothing -> IDENTIFIER
  return (Token tokenType lexeme Nothing 1)
  where
    findTokenMapping c = lookup c keywordsMapping
-- end scanToken()

-- Define the list of token scanners
scanners :: [Scanner Token]
scanners =
  [
    doubleCharToken,
    stringLiteral,
    numberLiteral,
    boolLiteral,      -- the order of scanners does matter
    identifier,       -- make sure that identifier is before singlechar     
    singleCharToken   -- so that Maximal munch principle is applied
  ]

-- Define a scanner to scan a list of tokens from the input string
scanTokens' :: Scanner [Token]
scanTokens' = many scanner

-- Combine all scanners into one
scanner :: Scanner Token
scanner = choice scanners

-- Function to scan tokens from an input string
scanTokens :: Source -> [Token]
scanTokens input =
  case scan scanTokens' input of
    [(tokens, _)] -> tokens
    _             -> []

-- > scanTokens "var foo = 123;"
-- [Token ...]
