module Main where

import System.IO
import System.Exit

import Token
import Scanner
import RecursiveParser
import AstPrinter

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
      repl
  else
    exit

parseAndPrintExpression :: [Token] -> String
parseAndPrintExpression tokens =
  case parseExpression tokens of
    Right expr -> astPrinter expr
    Left err -> "Error: " ++ show err

exit :: IO ()
exit = exitWith ExitSuccess
