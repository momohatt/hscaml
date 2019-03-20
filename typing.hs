{-# LANGUAGE TupleSections #-}

module Typing
    ( typeCheck
    , TypeErr(..)
    ) where

import Control.Monad.State
import Control.Exception
import Data.Maybe

import Syntax
import Type

data TypeErr = TypeErr String deriving (Show)
instance Exception TypeErr

tySchemaSubst :: Subst -> TySchema -> TySchema
tySchemaSubst s sc =
    (l, tySubst new_s t)
        where (l, t) = sc
              new_s = filter (\(v, t) -> v `elem` l) s

tyEnvSubst :: Subst -> TyEnv -> TyEnv
tyEnvSubst s =
    map (\(n, sc) -> (n, tySchemaSubst s sc))

getTyVars :: Ty -> [String]
getTyVars t =
    case t of
      TInt -> []
      TBool -> []
      TFun x y -> getTyVars x ++ getTyVars y
      TVar x -> [x]
      TTuple xs -> concatMap getTyVars xs
      TList x -> getTyVars x

removeDups :: Ord a => [a] -> [a]
removeDups l =
    case l of
      [] -> []
      x : xl -> x : removeDups (filter (/= x) xl)

generalize :: TyEnv -> Ty -> TySchema
generalize env t =
    (removeDups l3, t)
        where l1 = getTyVars t
              l2 = concatMap (getTyVars . snd . snd) env
              l3 = filter (`notElem` l2) l1

instantiate :: TySchema -> State Int Ty
instantiate (l, t) = instantiate' l t

instantiate' :: [String] -> Ty -> State Int Ty
instantiate' l t =
    case l of
      [] -> return t
      x : xl -> do
          y <- genNewTyVar
          let xl' = filter (/= x) xl
          instantiate' xl' (tySubst [(x, y)] t)

genNewTyVar :: State Int Ty
genNewTyVar = do
    i <- get
    put $ i + 1
    return $ TVar $ "t" ++ show i

-- generate constraint
genConst :: TyEnv -> Expr -> State Int (Ty, Constraint)
genConst env e =
    case e of
      EConstInt _  -> return (TInt, [])
      EConstBool _ -> return (TBool, [])
      EVar x  -> case lookup x env of
                   Just t -> (, []) <$> instantiate t
                   Nothing -> throw $ TypeErr $ "unknown type variable " ++ x
      ETuple es -> do
        ts <- mapM (genConst env) es
        let (tys, cons) = unzip ts
        return (TTuple tys, concat cons)
      ENil -> do
        t <- genNewTyVar
        return (TList t, [])
      ECons e1 e2 -> do
        (t1, c1) <- genConst env e1
        (t2, c2) <- genConst env e2
        return (TList t1, (TList t1, t2) : c1 ++ c2)
      ENot e' -> do
        (t, c) <- genConst env e'
        return (TBool, (t, TBool) : c)
      ENeg e' -> do
        (t, c) <- genConst env e'
        return (TInt, (t, TInt) : c)
      EBinop op e1 e2 -> do
        (t1, c1) <- genConst env e1
        (t2, c2) <- genConst env e2
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
      EFun x e -> do
        t <- genNewTyVar
        (t1, c1) <- genConst ((x, ([], t)) : env) e
        return (TFun t t1, c1)
      ELet x e1 e2 -> do
        (t1, c1) <- genConstDecl env (DLet x e1)
        (t2, c2) <- genConst ((x, t1) : env) e2
        return (t2, c1 ++ c2)
      ELetRec x e1 e2 -> do
        t <- genNewTyVar
        (t1, c1) <- genConstDecl ((x, ([], t)) : env) (DLet x e1)
        (t2, c2) <- genConst ((x, t1) : env) e2
        return (t2, c1 ++ c2)
      EApp f e -> do
        (t1, c1) <- genConst env f
        (t2, c2) <- genConst env e
        case t1 of
          TFun tf tt -> return (tt, (t2, tf) : c1 ++ c2)
          TVar t -> do
              tr <- genNewTyVar
              return (tr, (t1, TFun t2 tr) : c1 ++ c2)
      EMatch e ps -> do
        (t, c) <- genConst env e
        a <- genNewTyVar -- new type variable for expression
        (tp, te, cons) <- unzip3 <$> mapM helper ps
        let newcons = map (, t) tp ++ map (, a) te
        return (a, c ++ concat cons ++ newcons)
        where
          helper :: (Pattern, Expr) -> State Int (Ty, Ty, Constraint)
          helper (p, expr) = do
            (tp, cons, env') <- genConstPattern p
            (te, cons') <- genConst (env' ++ env) expr
            return (tp, te, cons ++ cons')

genConstPattern :: Pattern -> State Int (Ty, Constraint, TyEnv)
genConstPattern p =
    case p of
      PInt _ -> return (TInt, [], [])
      PBool _ -> return (TBool, [], [])
      PVar x -> do
        t <- genNewTyVar
        return (t, [], [(x, ([], t))])
      PTuple ts -> do
        (ts', cons', envs') <- unzip3 <$> mapM genConstPattern ts
        return (TTuple ts', concat cons', concat envs')
      PNil -> do
        t <- genNewTyVar
        return (t, [], [])
      PCons p1 p2 -> do
        (t1, c1, e1) <- genConstPattern p1
        (t2, c2, e2) <- genConstPattern p2
        t <- genNewTyVar
        return (TList t, [(t, t1), (TList t, t2)] ++ c1 ++ c2, e1 ++ e2)

genConstDecl :: TyEnv -> Decl -> State Int (TySchema, Constraint)
genConstDecl env d =
    case d of
      DLet x e -> do
          r <- genConst env e
          return $ helper r
      DLetRec f e -> do
          t <- genNewTyVar
          r <- genConst ((f, ([], t)) : env) e
          return $ helper r
    where
        helper :: (Ty, Constraint) -> (TySchema, Constraint)
        helper (t, c) =
          let sigma = tyUnify c
              t' = generalize env $ tySubst sigma t
           in (t', (t, snd t') : c)

tySubst :: Subst -> Ty -> Ty
tySubst s t =
    case t of
      TInt -> TInt
      TBool -> TBool
      TFun ts t -> TFun (tySubst s ts) (tySubst s t)
      TTuple ts -> TTuple $ map (tySubst s) ts
      TList t -> TList $ tySubst s t
      TVar x -> fromMaybe (TVar x) $ lookup x s

compose :: Subst -> Subst -> Subst
compose s1 s2 =
    s1 ++ map (\(v, t) -> (v, tySubst s1 t)) s2

checkFv :: String -> Ty -> Bool
checkFv v t =
    case t of
      TInt -> False
      TBool -> False
      TFun a b -> checkFv v a || checkFv v a
      TTuple ts -> any (checkFv v) ts
      TList t -> checkFv v t
      TVar a -> a == v

replaceFvInCons :: String -> Ty -> Constraint -> Constraint
replaceFvInCons v t =
    map (\(t1, t2) -> (tySubst [(v, t)] t1, tySubst [(v, t)] t2))

tyUnify :: Constraint -> Subst
tyUnify c =
    case c of
      [] -> []
      ts : xc ->
          case ts of
            (TInt, TInt) -> tyUnify xc
            (TBool, TBool) -> tyUnify xc
            (TFun s1 t1, TFun s2 t2) ->
                tyUnify $ [(s1, s2), (t1, t2)] ++ xc
            (TTuple ts1, TTuple ts2) ->
                if length ts1 /= length ts2
                   then throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                   else tyUnify $ zip ts1 ts2 ++ xc
            (TList t1, TList t2) ->
                tyUnify $ (t1, t2) : xc
            (TVar x1, TVar x2)
              | x1 == x2 -> tyUnify xc
              | otherwise ->
                  (`compose` [(x1, TVar x2)]) $ tyUnify newc
                      where newc = replaceFvInCons x1 (TVar x2) xc
            (TInt, TVar a) ->
                (`compose` [(a, TInt)]) $ tyUnify newc
                    where newc = replaceFvInCons a TInt xc
            (TVar a, TInt) ->
                (`compose` [(a, TInt)]) $ tyUnify newc
                    where newc = replaceFvInCons a TInt xc
            (TBool, TVar a) ->
                (`compose` [(a, TBool)]) $ tyUnify newc
                    where newc = replaceFvInCons a TBool xc
            (TVar a, TBool) ->
                (`compose` [(a, TBool)]) $ tyUnify newc
                    where newc = replaceFvInCons a TBool xc
            (TFun t1 t2, TVar a) ->
                case checkFv a (TFun t1 t2) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TFun t1 t2)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            (TVar a, TFun t1 t2) ->
                case checkFv a (TFun t1 t2) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TFun t1 t2)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            (TTuple t1, TVar a) ->
                case checkFv a (TTuple t1) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TTuple t1)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TTuple t1) xc
            (TVar a, TTuple t1) ->
                case checkFv a (TTuple t1) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TTuple t1)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TTuple t1) xc
            (TList t1, TVar a) ->
                case checkFv a (TList t1) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TList t1)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TList t1) xc
            (TVar a, TList t1) ->
                case checkFv a (TList t1) of
                  True -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TList t1)]) $ tyUnify newc
                               where newc = replaceFvInCons a (TList t1) xc
            _ -> throw $ TypeErr $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)

typeCheck :: Int -> TyEnv -> Command -> (Ty, TyEnv, Subst, Int)
typeCheck n tenv c =
    case c of
      CExpr e ->
          let (r, n') = runState (genConst tenv e) n
              (ts, const) = r
              s = tyUnify const
           in (tySubst s ts, tenv, s, n')
      CDecl d ->
          let (r, n') = runState (genConstDecl tenv d) n
              (ts, const) = r
              sigma = tyUnify const
              ts' = tySchemaSubst sigma ts
           in (snd ts', (nameOfDecl d, ts') : tenv, sigma, n')
