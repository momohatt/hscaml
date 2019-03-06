module Main where

import System.IO
import qualified Control.Monad (when)

import Parser (parseString)
import Eval (eval)

main :: IO()
main = do
    putStr "$ " >> hFlush stdout
    input <- getLine
    if input == "quit"
        then
            return ()
        else do
            print $ eval $ parseString input
            main
