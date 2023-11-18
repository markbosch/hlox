module AstPrinter(
    astPrinter
  ) where

import Expr
import Token
import Object

-- Pretty Printer
astPrinter :: Expr -> String
astPrinter (Unary token expr)        = parenthesize (printToken token) [expr]
astPrinter (Literal obj)             = printObject obj
astPrinter (Binary left token right) = parenthesize (printToken token) [left, right]
astPrinter (Grouping expr)           = parenthesize "group" [expr]

printObject :: Object -> String
printObject (Number num) = show num
printObject (String str) = show str
printObject (Bool bool)  = show bool

printToken :: Token -> String
printToken (Token _ lexeme _ _) = lexeme 

parenthesize :: String -> [Expr] -> String
parenthesize name exprs =
  "(" ++ name ++ " " ++ unwords (map astPrinter exprs) ++ ")"

-- Unary (Token.Token TokenType.MINUS "-" Nothing 1) $ Literal (Number 123)
-- Unary (Token.Token TokenType.MINUS "-" Nothing 1) $ Literal (String "123")
-- Binary (Unary (Token.Token TokenType.MINUS "-" Nothing 1) $ Literal $ Number 123) (Token.Token TokenType.STAR "*" Nothing 1) $ Grouping $ Literal $ Number 45.67
-- (* (- 123.0) (group 45.67)) 

-- astPrinter $ Unary (Token.Token TokenType.MINUS "-" Nothing 1) $ Literal (Number 123)
-- astPrinter $ Unary $ Literal $ Bool True
