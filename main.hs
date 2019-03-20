{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Data.Char (isSpace)
import System.Console.Haskeline
import System.IO

import Syntax
import Parser
import Eval
import Type
import Typing

initEnv = []
initTenv = []

isInputFinished :: String -> Bool
isInputFinished str =
    isInputFinished' $ reverse str

isInputFinished' str =
    case str of
      ';' : ';' : str' -> True
      c : str'
        | isSpace c -> isInputFinished' str'
        | otherwise -> False
      _             -> False

repl :: String -> Int -> TyEnv -> Env -> InputT IO ()
repl input' n tenv env = do
    minput <- getInputLine prompt
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "exit" -> return ()
      Just input ->
          if not $ isInputFinished input
            then repl (input' ++ input) n tenv env
            else do
              let parsedProg = parseString (input' ++ input)
              -- outputStrLn $ show parsedProg
                  (t, tenv', c, n) = typeCheck n tenv parsedProg
              -- outputStrLn $ show c -- for debug
              -- outputStrLn $ show tenv'
              -- outputStrLn $ show t
              deepseq c $ -- force tyUnify
                case parsedProg of
                  CExpr e -> do
                      outputStrLn $ "- : " ++ tyToStr t ++ " = " ++ valToStr (eval env e)
                      repl "" n tenv' env
                  CDecl e -> do
                      let (env', v) = evalDecl env e
                      outputStrLn $ "val " ++ nameOfDecl e ++ " : " ++ tyToStr t ++ " = " ++ valToStr v
                      repl "" n tenv' env'
              `catches`
              [ Handler $ \((EvalErr msg) :: EvalErr) -> do
                  outputStrLn $ "Eval error: " ++ msg
                  repl "" n tenv env
              , Handler $ \((TypeErr msg) :: TypeErr) -> do
                  outputStrLn $ "Type error: " ++ msg
                  repl "" n tenv env
              ]
      where
        prompt = if null input' then "# " else "  "

main :: IO ()
main =
    runInputT defaultSettings $ repl "" 0 initTenv initEnv
