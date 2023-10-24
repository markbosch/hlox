module Main where

import System.IO
import System.Exit

import Scanner

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
      print $ tokens
      repl
  else
    exit

exit :: IO ()
exit = exitWith ExitSuccess
