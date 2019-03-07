module Typing where

import Control.Monad.State

import Syntax
import Type

genNewTyVar :: State Int Ty
genNewTyVar = do
    i <- get
    put $ i + 1
    return $ TVar $ "t_" ++ show i

-- generate constraint
genConst :: TyEnv -> Expr -> State Int (Ty, Constraint)
genConst env e =
    case e of
      EConstInt _  -> return (TInt, [])
      EConstBool _ -> return (TBool, [])
      EVar x  -> case lookup x env of
                  Just t -> return (t, [])
      ENot e' -> do
          (t, c) <- genConst env e'
          return (TBool, (t, TBool) : c)
      ENeg e' -> do
          (t, c) <- genConst env e'
          return (TInt, (t, TInt) : c)
      EBinop op e1 e2 -> do
          (t1, c1) <- genConst env e1
          (t2, c2) <- genConst env e2
          -- Is there a better way to write this?
          case () of
            _ | op `elem` [BAnd, BOr] ->
                return (TBool, [(t1, TBool), (t2, TBool)] ++ c1 ++ c2)
              | op `elem` [BAdd, BSub, BMul, BDiv] ->
                return (TInt, [(t1, TInt), (t2, TInt)] ++ c1 ++ c2)
              | otherwise ->
                return (TBool, (t1, t2) : c1 ++ c2)
      EIf e1 e2 e3 -> do
          (t1, c1) <- genConst env e1
          (t2, c2) <- genConst env e2
          (t3, c3) <- genConst env e3
          return (t2, [(t1, TBool), (t2, t3)] ++ c1 ++ c2 ++ c3)
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

unify :: Constraint -> Maybe ()
unify c =
    case c of
      [] -> Just ()
      ts : xc ->
          case ts of
            (TInt, TInt) -> unify xc
            (TBool, TBool) -> unify xc

typeCheck :: Command -> IO ()
typeCheck c = do
    let body = case c of
                 CExpr e -> e
                 CDecl (DLet _ e) -> e
                 CDecl (DLetRec _ _ e) -> e
    let (_, const) = evalState (genConst [] body) 0
    case unify const of
      Just _ -> return ()
