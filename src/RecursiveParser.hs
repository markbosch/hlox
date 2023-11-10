module RecursiveParser
  ( parseExpression
  ) where

import Expr
import Token
import TokenType

newtype ParseError = ParseError String deriving (Show)

-- The parser
parseExpression :: [Token] -> Either ParseError Expr
parseExpression tokens = do
  case parseEquality tokens of
    (expr, []) -> Right expr
    (_, remaining) -> Left $ ParseError $ "Parsing error. Remaining tokens: " ++ show remaining

-- Equality parser
parseEquality :: [Token] -> (Expr, [Token])
parseEquality tokens =
  let (comparisonExpr, restTokens1) = parseComparison tokens
   in case restTokens1 of
        (Token BANG_EQUAL _ _ _ : rest) -> let (nextComparison, restTokens2) = parseComparison rest in (Binary comparisonExpr (head restTokens1) nextComparison, restTokens2)
        (Token EQUAL_EQUAL _ _ _ : rest) -> let (nextComparison, restTokens2) = parseComparison rest in (Binary comparisonExpr (head restTokens1) nextComparison, restTokens2)
        _ -> (comparisonExpr, restTokens1)

-- Comparison parser (similar structure for other non-terminals)
parseComparison :: [Token] -> (Expr, [Token])
parseComparison tokens =
  let (termExpr, restTokens1) = parseTerm tokens
   in case restTokens1 of
        (Token GREATER _ _ _ : rest) -> let (nextTerm, restTokens2) = parseTerm rest in (Binary termExpr (head restTokens1) nextTerm, restTokens2)
        (Token GREATER_EQUAL _ _ _ : rest) -> let (nextTerm, restTokens2) = parseTerm rest in (Binary termExpr (head restTokens1) nextTerm, restTokens2)
        (Token LESS _ _ _ : rest) -> let (nextTerm, restTokens2) = parseTerm rest in (Binary termExpr (head restTokens1) nextTerm, restTokens2)
        (Token LESS_EQUAL _ _ _ : rest) -> let (nextTerm, restTokens2) = parseTerm rest in (Binary termExpr (head restTokens1) nextTerm, restTokens2)
        _ -> (termExpr, restTokens1)

-- Term parser
parseTerm :: [Token] -> (Expr, [Token])
parseTerm tokens =
  let (factorExpr, restTokens1) = parseFactor tokens
   in case restTokens1 of
        (Token PLUS _ _ _ : rest) -> let (nextFactor, restTokens2) = parseFactor rest in (Binary factorExpr (head restTokens1) nextFactor, restTokens2)
        (Token MINUS _ _ _ : rest) -> let (nextFactor, restTokens2) = parseFactor rest in (Binary factorExpr (head restTokens1) nextFactor, restTokens2)
        _ -> (factorExpr, restTokens1)

-- Factor parser (similar structure for other non-terminals)
parseFactor :: [Token] -> (Expr, [Token])
parseFactor tokens =
  let (unaryExpr, restTokens1) = parseUnary tokens
   in case restTokens1 of
        (Token STAR _ _ _ : rest) -> let (nextUnary, restTokens2) = parseUnary rest in (Binary unaryExpr (head restTokens1) nextUnary, restTokens2)
        (Token SLASH _ _ _ : rest) -> let (nextUnary, restTokens2) = parseUnary rest in (Binary unaryExpr (head restTokens1) nextUnary, restTokens2)
        _ -> (unaryExpr, restTokens1)

-- Unary parser
parseUnary :: [Token] -> (Expr, [Token])
parseUnary tokens =
  case tokens of
    (Token BANG _ _ _ : rest) -> let (unaryExpr, restTokens) = parseUnary rest in (Unary (head tokens) unaryExpr, restTokens)
    (Token MINUS _ _ _ : rest) -> let (unaryExpr, restTokens) = parseUnary rest in (Unary (head tokens) unaryExpr, restTokens)
    _ -> parsePrimary tokens

-- Primary parser
parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary tokens =
  case tokens of
    (Token NUMBER lex _ _ : rest) -> (Literal (Number (read lex)), rest)
    (Token STRING lex _ _ : rest) -> (Literal (String lex), rest)
    (Token TRUE _ _ _ : rest) -> (Literal (Bool True), rest)
    (Token FALSE _ _ _ : rest) -> (Literal (Bool False), rest)
    (Token NIL _ _ _ : rest) -> (Literal Nil, rest)
    (Token IDENTIFIER lex _ _ : rest) -> (Variable (head tokens), rest)
    _ -> error "Unexpected token in primary expression"
