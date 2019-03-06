module Main where

import System.IO
import qualified Control.Monad (when)

import Syntax
import Parser (parseString)
import Eval (eval)

initEnv =
    [("a", VInt 100)]

main :: IO()
main = do
    putStr "$ " >> hFlush stdout
    input <- getLine
    if input == "quit"
        then
            return ()
        else do
            print $ eval initEnv $ parseString input
            -- print $ parseString input
            main
