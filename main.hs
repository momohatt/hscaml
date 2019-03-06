module Main where

import System.IO
import qualified Control.Monad (when)

import Syntax
import Parser (parseString)
import Eval (eval, evalDecl)

initEnv =
    [("a", VInt 100)]

repl :: Env -> IO()
repl env = do
    putStr "# " >> hFlush stdout
    input <- getLine
    if input == "quit"
       then
            return ()
        else do
            let input' = parseString input
            print input'
            case input' of
              CExpr e -> do
                  print $ eval env e
                  repl env
              CDecl e ->
                  repl (evalDecl env e)

main :: IO ()
main =
    repl initEnv
