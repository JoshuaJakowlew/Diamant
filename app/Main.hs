module Main where

import Expressions
import Translator

compile :: String -> String
compile s = case translate $ tokenize s of
    Just cpp -> cpp
    Nothing  -> "Syntax error!"


main :: IO ()
main = do
    input <- readFile "diamant.dmt"
    putStrLn $ compile input
