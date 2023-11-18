module Main where

import System.IO
import System.Exit

import Token
import Scanner
import RecursiveParser
import AstPrinter
import Interpreter

main :: IO ()
main = repl

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStr "lox> "
  input <- getLine
  if input /= "exit"
    then do
      let tokens = scanTokens input
      print tokens
      let result = parseAndPrintExpression tokens
      print result
      let foo = interpreter tokens
      print foo
      repl
  else
    exit

interpreter :: [Token] -> String
interpreter tokens = case expression $ tokens of
   Right expr -> case evaluate expr of
       Right result -> "Result: " ++ show result
       Left error -> "Error: " ++ error
   Left parseError -> "Parse error: " ++ show parseError


parseAndPrintExpression :: [Token] -> String
parseAndPrintExpression tokens =
  case expression tokens of
    Right expr -> astPrinter expr
    Left err -> "Error: " ++ show err

exit :: IO ()
exit = exitWith ExitSuccess
