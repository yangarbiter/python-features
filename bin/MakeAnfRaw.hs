module Main ( main
            ) where

import Anf
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [s] -> putStrLn $ processLine s
    _   -> error "Usage: make-anf CODE"

processLine :: String -> String
processLine s = case makeAnfSource s of
  Left err -> error "ANF parse error"
  Right newS -> newS
