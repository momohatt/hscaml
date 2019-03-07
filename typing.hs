{-# LANGUAGE TupleSections #-}

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

tySubst :: Subst -> Ty -> Either String Ty
tySubst s t =
    case t of
      TFun ts t ->
          TFun <$> mapM (tySubst s) ts <*> tySubst s t
      TVar x -> case lookup x s of
                  Just t -> Right t
                  _ -> Left "Variable not in scope"
      _ -> Right t

compose :: Subst -> Subst -> Either String Subst
compose s1 s2 =
    case mapM (\(v, t) -> (v,) <$> tySubst s1 t) s2 of -- <- TupleSections
      Right s2' -> Right $ s1 ++ s2'
      l -> l

checkFv :: Ty -> String -> Bool
checkFv t v =
    case t of
      TInt -> False
      TBool -> False
      TFun a b -> any (`checkFv` v) a || checkFv b v
      TVar a -> a == v

replaceFvInCons :: String -> Ty -> Constraint -> Either String Constraint
replaceFvInCons v t =
    mapM (\(t1, t2) -> (,) <$> tySubst [(v, t)] t1 <*> tySubst [(v, t)] t2)

tyUnify :: Constraint -> Either String Subst
tyUnify c =
    case c of
      [] -> Right []
      ts : xc ->
          case ts of
            (TInt, TInt) -> tyUnify xc
            (TBool, TBool) -> tyUnify xc
            (TFun s1 t1, TFun s2 t2) ->
                tyUnify $ zip s1 s2 ++ [(t1, t2)] ++ xc
            (TVar x1, TVar x2)
              | x1 == x2 -> tyUnify xc
              | otherwise -> ((x1, TVar x2) :) <$> tyUnify xc
            (TInt, TVar a) ->
                newc >>= tyUnify >>= (`compose` [(a, TInt)])
                    where newc = replaceFvInCons a TInt xc
            (TVar a, TInt) ->
                newc >>= tyUnify >>= (`compose` [(a, TInt)])
                    where newc = replaceFvInCons a TInt xc
            (TBool, TVar a) ->
                newc >>= tyUnify >>= (`compose` [(a, TBool)])
                    where newc = replaceFvInCons a TBool xc
            (TVar a, TBool) ->
                newc >>= tyUnify >>= (`compose` [(a, TBool)])
                    where newc = replaceFvInCons a TBool xc
            (TFun t1 t2, TVar a) ->
                case checkFv (TFun t1 t2) a of
                  True -> Left "violating occurrence rule"
                  False -> newc >>= tyUnify >>= (`compose` [(a, TFun t1 t2)])
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            (TVar a, TFun t1 t2) ->
                case checkFv (TFun t1 t2) a of
                  True -> Left "violating occurrence rule"
                  False -> newc >>= tyUnify >>= (`compose` [(a, TFun t1 t2)])
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            _ -> Left ("cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts))

typeCheck :: TyEnv -> Command -> Either String (Ty, TyEnv)
typeCheck tenv c =
    case c of
      CExpr e ->
          case tyUnify const of
            Right s -> (, tenv) <$> tySubst s t
            Left msg -> Left msg
            where (t, const) = evalState (genConst tenv e) 0
                  s = tyUnify const
