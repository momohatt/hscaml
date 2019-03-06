module Typing where

import Control.Monad.State

import Syntax
import Type

genNewTyVar :: State Int Ty
genNewTyVar = do
    i <- get
    put $ i + 1
    return $ TVar $ "t_" ++ show i

genConst :: TyEnv -> Expr -> State Int (Ty, Constraint)
genConst env e =
    case e of
      EConstInt _  -> return (TInt, [])
      EConstBool _ -> return (TBool, [])
      EVar x  -> case lookup x env of
                  Just t -> return (t, [])
      ENot e' -> do
          (t, c) <- genConst env e'
          case t of
            TBool -> return (TBool, c)
      ENeg e' -> do
          (t, c) <- genConst env e'
          case t of
            TInt -> return (TInt, c)
      EBinop op e1 e2 -> do
          (t1, c1) <- genConst env e1
          (t2, c2) <- genConst env e2
          case (t1, t2) of
            (TBool, TBool)
              | op `notElem` [BAdd, BSub, BMul, BDiv] -> return (TBool, c1 ++ c2)
            (TInt, TInt)
              | op `notElem` [BAnd, BOr] -> return (TInt, c1 ++ c2)
      EIf e1 e2 e3 -> do
          (t1, c1) <- genConst env e1
          (t2, c2) <- genConst env e2
          (t3, c3) <- genConst env e3
          case t1 of
            TBool -> return (t2, (t2, t3) : c1 ++ c2 ++ c3)
      ELet x e1 e2 -> do
          (t1, c1) <- genConst env e1
          (t2, c2) <- genConst ((x, t1) : env) e2
          return (t2, c1 ++ c2)
      ELetRec f xs e1 e2 -> do
          ts <- mapM (const genNewTyVar) xs
          tr <- genNewTyVar
          let env' = (f, TFun ts tr) : env
          (t1, c1) <- genConst (zip xs ts ++ env') e1
          (t2, c2) <- genConst env' e2
          return (TFun ts tr, (tr, t1) : c1 ++ c2)
      EApp f es -> do
          (tf, cf) <- genConst env (EVar f)
          (ts, cs) <- unzip <$> mapM (genConst env) es
          case tf of
            TFun tfa tfr ->
                return (tfr, zip tfa ts ++ cf ++ concat cs)
