{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char                        (isSpace)

import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (except, ExceptT, runExceptT)
import Control.Monad.Trans.State.Lazy

import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)

import Syntax
import Parser
import IState
import Eval
import Type
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

addHistory :: String -> InputT IO ()
addHistory input = do
  history <- getHistory
  putHistory $ addHistoryUnlessConsecutiveDupe input history

readInput :: String -> InputT IO (Maybe String)
readInput input' = do
  let prompt = if null input' then "# " else "  "
  minput <- getInputLine prompt
  case minput of
    Nothing -> return Nothing
    Just "quit" -> return Nothing
    Just "exit" -> return Nothing
    Just input | not (isInputFinished input) -> do
      addHistory input
      readInput (input' ++ input ++ " ")
    Just input -> do
      addHistory input
      return (Just (input' ++ input))

repl :: IStateT (InputT IO) ()
repl = do
  minput <- lift $ readInput ""
  case minput of
    Nothing -> return ()
    Just input -> do
      ety <- runExceptT (processCmd input)
      case ety of
        Left msg -> lift $ outputStrLn msg
        Right msg -> lift $ outputStrLn msg
      repl
  where
    processCmd :: String -> ExceptT String (IStateT (InputT IO)) String
    processCmd input = do
      prog <- except (parseCmd input)
      (ty, _) <- typeCheck prog
      v <- evalCmd prog
      case prog of
        CExpr _ ->
          return $ "- : " ++ show ty ++ " = " ++ show v
        CDecl e ->
          return $ "val " ++ nameOfDecl e ++ " : " ++ show ty ++ " = " ++ show v

haskelineSettings :: Settings IO
haskelineSettings =
  Settings { complete       = completeFilename
           , historyFile    = Nothing
           , autoAddHistory = False
           }

main :: IO ()
main =
  runInputT haskelineSettings . (`evalStateT` initIState) $ repl
