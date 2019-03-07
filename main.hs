module Main where

import System.IO
import Control.Exception

import Syntax
import Parser (parseString)
import Eval (eval, evalDecl)
import Type
import Typing

initEnv = [("a", VInt 100)]
initTenv = [("a", TInt)]

repl :: TyEnv -> Env -> IO()
repl tenv env = do
    putStr "# " >> hFlush stdout
    input <- getLine
    if input == "quit"
       then
            return ()
        else do
            let parsedProg = parseString input
            print parsedProg
            case typeCheck tenv parsedProg of
              Left msg -> do
                  putStrLn ("TypeError: " ++ msg)
                  repl tenv env
              Right (t, tenv') -> do
                print t
                case parsedProg of
                  CExpr e -> do
                      print $ eval env e
                      repl tenv' env
                  CDecl e ->
                      repl tenv' (evalDecl env e)

main :: IO ()
main =
    repl initTenv initEnv
