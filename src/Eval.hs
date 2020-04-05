module Eval
  ( evalCmd
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

import Syntax
import IState

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

eval :: Monad m => IState -> Expr -> ExceptT String m Value
eval st e =
  case e of
    EConstInt n -> return (VInt n)
    EConstBool b -> return (VBool b)
    EVar x ->
      case lookup x (env st) of
        Just v -> return v
        Nothing -> throwE $ "Unknown variable " ++ x
    ETuple es ->
      VTuple <$> mapM (eval st) es
    ENil ->
      return VNil
    ECons e1 e2 ->
      VCons <$> eval st e1 <*> eval st e2
    ENot e' -> do
      v <- eval st e'
      case v of
        VBool b -> return (VBool (not b))
    ENeg e' -> do
      v <- eval st e'
      case v of
        VInt n -> return (VInt (- n))
    EBinop op e1 e2 ->
      evalBinOp op <$> eval st e1 <*> eval st e2
    EIf e1 e2 e3 -> do
      v <- eval st e1
      case v of
        VBool b -> eval st (if b then e2 else e3)
    EFun x e ->
      return (VFun x e (env st))
    ELetIn d1 e2 -> do
      (_, st') <- evalDecl st d1
      eval st' e2
    EApp f e -> do
      z <- eval st e
      v <- eval st f
      case v of
        VFun x body env' -> eval (st { env = (x, z) : env' }) body
    EMatch e ps -> do
      val <- eval st e
      (env', expr) <- helper ps val
      eval (st { env = env' ++ env st }) expr
        where
          helper :: Monad m => [(Pattern, Expr)] -> Value -> ExceptT String m (Env, Expr)
          helper ps val = case ps of
            [] -> throwE "Match Failure"
            (p, exp) : px -> case findMatch p val of
              Just env -> return (env, exp)
              Nothing -> helper px val

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

evalDecl :: Monad m => IState -> Decl -> ExceptT String m (Value, IState)
evalDecl st (DLet x e) = do
  val <- eval st e
  return (val, st { env = (x, val) : env st })
evalDecl st (DLetRec f (EFun x e')) = do
  let env' = (f, VFun x e' env') : env st
  let val = VFun x e' env'
  return (val, st { env = (f, val) : env st })

evalCmd :: Monad m => Command -> ExceptT String (IStateT m) Value
evalCmd (CExpr e) = do
  st <- lift get
  eval st e
evalCmd (CDecl e) = do
  st <- lift get
  (val, st') <- evalDecl st e
  lift (put st')
  return val
