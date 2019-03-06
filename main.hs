module Main where

import System.IO
import qualified Control.Monad (when)

import Parser (parseString)

main :: IO()
main = do
    putStr "$ " >> hFlush stdout
    input <- getLine
    if input == "quit"
        then
            return ()
        else do
            print $ parseString input
            main
