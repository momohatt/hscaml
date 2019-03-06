module Eval where

import Syntax

eval :: Expr -> Value
eval e = case e of
           EConstInt n -> VInt n
           EConstBool b -> VBool b
           ENot e' ->
               case eval e' of
                 VBool b -> VBool $ not b
           EAnd e1 e2 -> boolEval (&&) e1 e2
           EOr e1 e2  -> boolEval (||) e1 e2
           ENeg e' ->
               case eval e' of
                 VInt n -> VInt $ -1 * n
           EAdd e1 e2 -> arithEval (+) e1 e2
           ESub e1 e2 -> arithEval (-) e1 e2
           EMul e1 e2 -> arithEval (*) e1 e2
           EDiv e1 e2 -> arithEval div e1 e2
           EEq e1 e2  -> relEval (==) e1 e2
           EGT e1 e2  -> relEval (>)  e1 e2
           ELT e1 e2  -> relEval (<)  e1 e2
           EGE e1 e2  -> relEval (>=) e1 e2
           ELE e1 e2  -> relEval (<=) e1 e2

boolEval :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Value
boolEval op e1 e2 =
    case (eval e1, eval e2) of
      (VBool b1, VBool b2) -> VBool $ op b1 b2

arithEval :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Value
arithEval op e1 e2 =
    case (eval e1, eval e2) of
      (VInt n1, VInt n2) -> VInt $ op n1 n2

relEval :: (Value -> Value -> Bool) -> Expr -> Expr -> Value
relEval r e1 e2 =
    VBool $ r (eval e1) (eval e2)
