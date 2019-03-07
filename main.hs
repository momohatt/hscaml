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

repl :: TyEnv -> Env -> IO ()
repl tenv env = do
    putStr "# " >> hFlush stdout
    input <- getLine
    if input == "quit"
        then
            return ()
        else do
            let parsedProg = parseString input
            -- print parsedProg
            case typeCheck tenv parsedProg of
              Left msg -> do
                  putStrLn ("TypeError: " ++ msg)
                  repl tenv env
              Right (t, tenv') ->
                -- print t
                case parsedProg of
                  CExpr e -> do
                      putStrLn $ "- : " ++ tyToStr t ++ " = " ++ valToStr (eval env e)
                      repl tenv' env
                  CDecl e -> do
                      let (env', v) = evalDecl env e
                      putStrLn $ "val " ++ nameOfDecl e ++ " : " ++ tyToStr t ++ " = " ++ valToStr v
                      repl tenv' env'

main :: IO ()
main =
    repl initTenv initEnv
