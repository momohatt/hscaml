{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char                        (isSpace)
import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)

import Syntax
import Parser
import IState
import Eval
import Typing

isInputFinished :: String -> Bool
isInputFinished = isInputFinished' . reverse
  where
    isInputFinished' str =
      case str of
        ';' : ';' : _ -> True
        c : str'
          | isSpace c -> isInputFinished' str'
          | otherwise -> False
        _             -> False

repl :: String -> IState -> InputT IO ()
repl input' st = do
  minput <- getInputLine prompt
  case minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "exit" -> return ()
    Just input -> do
      history <- getHistory
      putHistory $ addHistoryUnlessConsecutiveDupe input history
      if not $ isInputFinished input
        then repl (input' ++ input ++ " ") st
        else
          case parseCmd (input' ++ input) of
            Left msg -> do
              outputStr ("Parse error at: " ++ msg)
              repl "" st
            Right parsedProg ->
              case typeCheck st parsedProg of
                Left msg -> do
                  outputStrLn ("Type error: " ++ msg)
                  repl "" st
                Right (t, _, st') -> do
                  case parsedProg of
                    CExpr e -> do
                      outputStrLn $ "- : " ++ show t ++ " = " ++ show (eval (env st') e)
                      repl "" st'
                    CDecl e -> do
                      let (env', v) = evalDecl (env st') e
                      outputStrLn $ "val " ++ nameOfDecl e ++ " : " ++ show t ++ " = " ++ show v
                      repl "" (st' { env = env' })
          `catch`
          (\((EvalErr msg) :: EvalErr) -> do
            outputStrLn msg
            repl "" st)
    where
      prompt = if null input' then "# " else "  "

haskelineSettings :: Settings IO
haskelineSettings =
  Settings { complete       = completeFilename
           , historyFile    = Nothing
           , autoAddHistory = False
           }

main :: IO ()
main =
  runInputT haskelineSettings $ repl "" initIState
