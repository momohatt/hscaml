module Main where

import System.IO
import Control.Exception

import Syntax
import Parser (parseString)
import Eval (eval, evalDecl)
import Typing

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
            let parsedProg = parseString input
            print parsedProg
            typeCheck parsedProg
             `catch` \(PatternMatchFail _) -> do
                 putStrLn "TypeError."
                 repl env
            case parsedProg of
              CExpr e -> do
                  print $ eval env e
                  repl env
              CDecl e ->
                  repl (evalDecl env e)

main :: IO ()
main =
    repl initEnv
