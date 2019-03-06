module Eval where

import Syntax

eval :: Env -> Expr -> Value
eval env e =
    case e of
      EConstInt n -> VInt n
      EConstBool b -> VBool b
      EVar x ->
          case lookup x env of
            Just v -> v
      ENot e' ->
          case eval env e' of
            VBool b -> VBool $ not b
      EAnd e1 e2 -> boolEval (&&) env e1 e2
      EOr e1 e2  -> boolEval (||) env e1 e2
      ENeg e' ->
          case eval env e' of
            VInt n -> VInt $ -1 * n
      EAdd e1 e2 -> arithEval (+) env e1 e2
      ESub e1 e2 -> arithEval (-) env e1 e2
      EMul e1 e2 -> arithEval (*) env e1 e2
      EDiv e1 e2 -> arithEval div env e1 e2
      EEq e1 e2  -> relEval (==) env e1 e2
      EGT e1 e2  -> relEval (>)  env e1 e2
      ELT e1 e2  -> relEval (<)  env e1 e2
      EGE e1 e2  -> relEval (>=) env e1 e2
      ELE e1 e2  -> relEval (<=) env e1 e2
      EIf e1 e2 e3 ->
          case eval env e1 of
            VBool b -> if b then eval env e2 else eval env e3
      ELet x e1 e2 ->
          eval ((x, eval env e1):env) e2

boolEval :: (Bool -> Bool -> Bool) -> Env -> Expr -> Expr -> Value
boolEval op env e1 e2 =
    case (eval env e1, eval env e2) of
      (VBool b1, VBool b2) -> VBool $ op b1 b2

arithEval :: (Integer -> Integer -> Integer) -> Env -> Expr -> Expr -> Value
arithEval op env e1 e2 =
    case (eval env e1, eval env e2) of
      (VInt n1, VInt n2) -> VInt $ op n1 n2

relEval :: (Value -> Value -> Bool) -> Env -> Expr -> Expr -> Value
relEval r env e1 e2 =
    VBool $ r (eval env e1) (eval env e2)

evalDecl :: Env -> Decl -> Env
evalDecl env e =
    case e of
      DLet x e' ->
          (x, eval env e'):env
