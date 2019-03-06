module Eval where

import Syntax

eval :: Expr -> Value
eval e = case e of
           EConstInt n -> VInt n
           EConstBool b -> VBool b
           ENeg e' -> case eval e' of
                        VInt n -> VInt (-1 * n)
           EAdd e1 e2 -> case (eval e1, eval e2) of
                           (VInt n1, VInt n2) -> VInt (n1 + n2)
           ESub e1 e2 -> case (eval e1, eval e2) of
                           (VInt n1, VInt n2) -> VInt (n1 - n2)
           EMul e1 e2 -> case (eval e1, eval e2) of
                           (VInt n1, VInt n2) -> VInt (n1 * n2)
           EDiv e1 e2 -> case (eval e1, eval e2) of
                           (VInt n1, VInt n2) -> VInt (n1 `div` n2)
