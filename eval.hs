module Eval
    ( eval
    , evalDecl
    ) where

import Control.Monad

import Syntax

findMatch :: Pattern -> Value -> Maybe Env
findMatch p v = case (p, v) of
    (PInt a, VInt b)       -> if a == b then return [] else Nothing
    (PBool a, VBool b)     -> if a == b then return [] else Nothing
    (PVar a, _)            -> return [(a, v)]
    (PTuple ps, VTuple vs) ->
      if length ps /= length vs then Nothing
                                else concat <$> zipWithM findMatch ps vs
    (PNil, VNil)           -> return []
    (PCons h t, VCons h' t') ->
      (++) <$> findMatch h h' <*> findMatch t t'

eval :: Env -> Expr -> Value
eval env e =
    case e of
      EConstInt n -> VInt n
      EConstBool b -> VBool b
      EVar x ->
          case lookup x env of
            Just v -> v
      ETuple es ->
          VTuple $ map (eval env) es
      ENil ->
          VNil
      ECons e1 e2 ->
          VCons (eval env e1) (eval env e2)
      ENot e' ->
          case eval env e' of
            VBool b -> VBool $ not b
      ENeg e' ->
          case eval env e' of
            VInt n -> VInt $ -1 * n
      EBinop op e1 e2 ->
          evalBinOp op (eval env e1) (eval env e2)
      EIf e1 e2 e3 ->
          case eval env e1 of
            VBool b -> if b then eval env e2 else eval env e3
      EFun x e ->
          VFun x e env
      ELet x e1 e2 ->
          eval ((x, eval env e1) : env) e2
      ELetRec f e1 e2 ->
          case e1 of
            EFun x e' ->
                let env' = (f, VFun x e' env') : env
                 in eval env' e2
      EApp f e ->
          let z = eval env e in
          case eval env f of
            VFun x body env' -> eval ((x, z) : env') body
      EMatch e ps ->
          let (env', expr) = helper ps in
          eval (env' ++ env) expr
            where
              val = eval env e
              helper :: [(Pattern, Expr)] -> (Env, Expr)
              helper ps = case ps of
                  [] -> error "Match failure"
                  (p, exp) : px -> case findMatch p val of
                      Just env -> (env, exp)
                      Nothing -> helper px

evalBinOp :: Binop -> Value -> Value -> Value
evalBinOp op =
    case op of
      BAnd -> \ (VBool b1) (VBool b2) -> VBool $ b1 && b2
      BOr  -> \ (VBool b1) (VBool b2) -> VBool $ b1 || b2
      BAdd -> \ (VInt n1) (VInt n2)   -> VInt  $ n1 + n2
      BSub -> \ (VInt n1) (VInt n2)   -> VInt  $ n1 - n2
      BMul -> \ (VInt n1) (VInt n2)   -> VInt  $ n1 * n2
      BDiv -> \ (VInt n1) (VInt n2)   -> VInt  $ n1 `div` n2
      -- we don't need pattern-match for the arguments after type matching
      BEq  -> \ e1 e2 -> case (e1, e2) of (VInt a, VInt b)   -> VBool $ a == b
                                          (VBool a, VBool b) -> VBool $ a == b
      BGT  -> \ e1 e2 -> case (e1, e2) of (VInt a, VInt b)   -> VBool $ a > b
                                          (VBool a, VBool b) -> VBool $ a > b
      BLT  -> \ e1 e2 -> case (e1, e2) of (VInt a, VInt b)   -> VBool $ a < b
                                          (VBool a, VBool b) -> VBool $ a < b
      BGE  -> \ e1 e2 -> case (e1, e2) of (VInt a, VInt b)   -> VBool $ a >= b
                                          (VBool a, VBool b) -> VBool $ a >= b
      BLE  -> \ e1 e2 -> case (e1, e2) of (VInt a, VInt b)   -> VBool $ a <= b
                                          (VBool a, VBool b) -> VBool $ a <= b

evalDecl :: Env -> Decl -> (Env, Value)
evalDecl env e =
    case e of
      DLet x e ->
          ((x, val) : env, val)
              where val = eval env e
      DLetRec f e ->
          case e of
            EFun x e' ->
                let env' = (f, VFun x e' env') : env
                    val = VFun x e' env'
                 in ((f, val) : env, val)
