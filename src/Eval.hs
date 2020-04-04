module Eval
  ( eval
  , evalDecl
  , EvalErr(..)
  ) where

import Control.Exception
import Control.Monad

import Syntax
import IState

data EvalErr = EvalErr String deriving (Show)
instance Exception EvalErr

findMatch :: Pattern -> Value -> Maybe Env
findMatch (PInt a)  (VInt b)  | a == b = return []
findMatch (PBool a) (VBool b) | a == b = return []
findMatch (PVar a)  v                  = return [(a, v)]
findMatch (PTuple ps) (VTuple vs) | length ps == length vs =
  concat <$> zipWithM findMatch ps vs
findMatch PNil VNil = return []
findMatch (PCons h t) (VCons h' t') =
  (++) <$> findMatch h h' <*> findMatch t t'
findMatch _ _ = Nothing

eval :: IState -> Expr -> Value
eval st e =
  case e of
    EConstInt n -> VInt n
    EConstBool b -> VBool b
    EVar x ->
      case lookup x (env st) of
        Just v -> v
        Nothing -> throw $ EvalErr $ "Unknown variable " ++ x
    ETuple es ->
      VTuple $ map (eval st) es
    ENil ->
      VNil
    ECons e1 e2 ->
      VCons (eval st e1) (eval st e2)
    ENot e' ->
      case eval st e' of
        VBool b -> VBool $ not b
    ENeg e' ->
      case eval st e' of
        VInt n -> VInt $ -1 * n
    EBinop op e1 e2 ->
      evalBinOp op (eval st e1) (eval st e2)
    EIf e1 e2 e3 ->
      case eval st e1 of
        VBool b -> if b then eval st e2 else eval st e3
    EFun x e ->
      VFun x e (env st)
    ELetIn d1 e2 ->
      let (_, st') = evalDecl st d1
       in eval st' e2
    EApp f e ->
      let z = eval st e in
      case eval st f of
        VFun x body env' -> eval (st { env = (x, z) : env' }) body
    EMatch e ps ->
      let (env', expr) = helper ps in
      eval (st { env = env' ++ env st }) expr
        where
          val = eval st e
          helper :: [(Pattern, Expr)] -> (Env, Expr)
          helper ps = case ps of
            [] -> throw $ EvalErr "Match Failure"
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

evalDecl :: IState -> Decl -> (Value, IState)
evalDecl st (DLet x e) =
  let val = eval st e
   in (val, st { env = (x, val) : env st })
evalDecl st (DLetRec f (EFun x e')) =
  let env' = (f, VFun x e' env') : env st
      val = VFun x e' env'
   in (val, st { env = (f, val) : env st })
