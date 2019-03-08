module Main where

import System.IO
import Control.Exception

import Syntax
import Parser (parseString)
import Eval (eval, evalDecl)
import Type
import Typing

initEnv = []
initTenv = []

repl :: Int -> TyEnv -> Env -> IO ()
repl n tenv env = do
    putStr "# " >> hFlush stdout
    input <- getLine
    if input == "quit"
        then
            return ()
        else do
            let parsedProg = parseString input
            print parsedProg
            case typeCheck n tenv parsedProg of
              Left msg -> do
                  putStrLn ("TypeError: " ++ msg)
                  repl n tenv env
              Right (t, tenv', n) -> do
                print tenv'
                -- print t
                case parsedProg of
                  CExpr e -> do
                      putStrLn $ "- : " ++ tyToStr t ++ " = " ++ valToStr (eval env e)
                      repl n tenv' env
                  CDecl e -> do
                      let (env', v) = evalDecl env e
                      putStrLn $ "val " ++ nameOfDecl e ++ " : " ++ tyToStr t ++ " = " ++ valToStr v
                      repl n tenv' env'

main :: IO ()
main =
    repl 0 initTenv initEnv
