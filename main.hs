module Main where

import Control.Exception
import System.Console.Haskeline
import System.IO

import Syntax
import Parser
import Eval
import Type
import Typing

initEnv = []
initTenv = []

repl :: Int -> TyEnv -> Env -> InputT IO ()
repl n tenv env = do
    minput <- getInputLine "# "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "exit" -> return ()
      Just input -> do
          let parsedProg = parseString input
          -- print parsedProg
          case typeCheck n tenv parsedProg of
            Left msg -> do
                outputStrLn ("TypeError: " ++ msg)
                repl n tenv env
            Right (t, tenv', c, n) -> do
              -- print c -- for debug
              -- print tenv'
              -- print t
              case parsedProg of
                CExpr e -> do
                    outputStrLn $ "- : " ++ tyToStr t ++ " = " ++ valToStr (eval env e)
                    repl n tenv' env
                CDecl e -> do
                    let (env', v) = evalDecl env e
                    outputStrLn $ "val " ++ nameOfDecl e ++ " : " ++ tyToStr t ++ " = " ++ valToStr v
                    repl n tenv' env'

main :: IO ()
main =
    runInputT defaultSettings $ repl 0 initTenv initEnv
