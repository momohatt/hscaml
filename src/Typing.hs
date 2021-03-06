{-# LANGUAGE TupleSections #-}

module Typing
  ( typeCheck
  ) where

import Control.Monad.State
import Control.Monad.Trans.Except       (catchE, except, ExceptT, throwE)
import Data.Maybe
import Data.Either

import Syntax
import IState
import Type

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
genConst :: TyEnv -> Expr -> State Int (Either String (Ty, Constraint))
genConst env e =
  case e of
    EConstInt _  -> return $ Right (TInt, [])
    EConstBool _ -> return $ Right (TBool, [])
    EVar x  -> case lookup x env of
                Just t -> Right . (, []) <$> instantiate t
                Nothing -> return $ Left $ "unknown type variable " ++ x
    ETuple es -> do
      ts <- mapM (genConst env) es
      if any isLeft ts
         then return $ Left $ head $ lefts ts
         else do
             let (tys, cons) = unzip $ rights ts
             return $ Right (TTuple tys, concat cons)
    ENil -> do
      t <- genNewTyVar
      return $ Right (TList t, [])
    ECons e1 e2 -> do
      r1 <- genConst env e1
      r2 <- genConst env e2
      return $ (\(t1, c1) (t2, c2) ->
        (TList t1, (TList t1, t2) : c1 ++ c2)) <$> r1 <*> r2
    ENot e' -> do
      r <- genConst env e'
      return $ (\(t, c) -> (TBool, (t, TBool) : c)) <$> r
    ENeg e' -> do
      r <- genConst env e'
      return $ (\(t, c) -> (TInt, (t, TInt) : c)) <$> r
    EBinop op e1 e2 -> do
      r1 <- genConst env e1
      r2 <- genConst env e2
      return $ (\(t1, c1) (t2, c2) ->
        case () of
          _ | op `elem` [BAnd, BOr] ->
              (TBool, [(t1, TBool), (t2, TBool)] ++ c1 ++ c2)
            | op `elem` [BAdd, BSub, BMul, BDiv] ->
              (TInt, [(t1, TInt), (t2, TInt)] ++ c1 ++ c2)
            | otherwise ->
              (TBool, (t1, t2) : c1 ++ c2)) <$> r1 <*> r2
    EIf e1 e2 e3 -> do
      r1 <- genConst env e1
      r2 <- genConst env e2
      r3 <- genConst env e3
      return $ (\(t1, c1) (t2, c2) (t3, c3) ->
        (t2, [(t1, TBool), (t2, t3)] ++ c1 ++ c2 ++ c3)) <$> r1 <*> r2 <*> r3
    EFun x e -> do
      t <- genNewTyVar
      r <- genConst ((x, ([], t)) : env) e
      return $ (\(t1, c1) -> (TFun t t1, c1)) <$> r

    ELetIn d1 e2 -> do
      r1 <- genConstDecl env d1
      case r1 of
        Left msg -> return $ Left msg
        Right (t1, c1, env') -> do
          r2 <- genConst env' e2
          return $ (\(t2, c2) -> (t2, c1 ++ c2)) <$> r2
    EApp f e -> do
      r1 <- genConst env f
      r2 <- genConst env e
      case (r1, r2) of
        (Right (t1, c1), Right (t2, c2)) ->
          case t1 of
            TFun tf tt -> return $ Right (tt, (t2, tf) : c1 ++ c2)
            TVar t -> do
                tr <- genNewTyVar
                return $ Right (tr, (t1, TFun t2 tr) : c1 ++ c2)
        (Left _, _) -> return r1
        (_, Left _) -> return r2
    EMatch e ps -> do
      r <- genConst env e
      case r of
        Left msg -> return $ Left msg
        Right (t, c) -> do
          a <- genNewTyVar -- new type variable for expression
          tmp <- mapM helper ps
          case sequenceA tmp of
            Left msg -> return $ Left msg
            Right list -> do
              let (tp, te, cons) = unzip3 list
                  newcons = map (, t) tp ++ map (, a) te
              return $ Right (a, c ++ concat cons ++ newcons)
        where
          helper :: (Pattern, Expr) -> State Int (Either String (Ty, Ty, Constraint))
          helper (p, expr) = do
            (tp, cons, env') <- genConstPattern p
            r <- genConst (env' ++ env) expr
            return $ (\(te, cons') -> (tp, te, cons ++ cons')) <$> r

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

genConstDecl :: TyEnv -> Decl -> State Int (Either String (TySchema, Constraint, TyEnv))
genConstDecl env d =
  case d of
    DLet x e -> do
      r <- genConst env e
      return $ helper x env r
    DLetRec f e -> do
      t <- genNewTyVar
      r <- genConst ((f, ([], t)) : env) e
      return $ helper f env r
  where
    helper :: String -> TyEnv -> Either String (Ty, Constraint) -> Either String (TySchema, Constraint, TyEnv)
    helper x env r = do
      (t, c) <- r
      sigma <- tyUnify c
      let t' = generalize env $ tySubst sigma t
      return (t', (t, snd t') : c, (x, t') : env)

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
  map (\(v, t) -> (v, tySubst s1 t)) s2 ++ s1

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

tyUnify :: Constraint -> Either String Subst
tyUnify c =
  case c of
    [] -> Right []
    ts : xc ->
      case ts of
        (TInt, TInt) -> tyUnify xc
        (TBool, TBool) -> tyUnify xc
        (TFun s1 t1, TFun s2 t2) ->
          tyUnify $ [(s1, s2), (t1, t2)] ++ xc
        (TTuple ts1, TTuple ts2) ->
          if length ts1 /= length ts2
             then Left $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
             else tyUnify $ zip ts1 ts2 ++ xc
        (TList t1, TList t2) ->
          tyUnify $ (t1, t2) : xc
        (TVar x1, TVar x2)
          | x1 == x2 -> tyUnify xc
          | otherwise ->
            (`compose` [(x1, TVar x2)]) <$> tyUnify newc
                where newc = replaceFvInCons x1 (TVar x2) xc
        (TInt, TVar a) ->
          (`compose` [(a, TInt)]) <$> tyUnify newc
              where newc = replaceFvInCons a TInt xc
        (TVar a, TInt) ->
          (`compose` [(a, TInt)]) <$> tyUnify newc
              where newc = replaceFvInCons a TInt xc
        (TBool, TVar a) ->
          (`compose` [(a, TBool)]) <$> tyUnify newc
              where newc = replaceFvInCons a TBool xc
        (TVar a, TBool) ->
          (`compose` [(a, TBool)]) <$> tyUnify newc
              where newc = replaceFvInCons a TBool xc

        (t1@(TVar a), t2) | checkFv a t2 ->
          Left $ "cannot unify " ++ show t1 ++ " and " ++ show t2
        (t1, t2@(TVar a)) | checkFv a t1 ->
          Left $ "cannot unify " ++ show t1 ++ " and " ++ show t2

        (TFun t1 t2, TVar a) ->
          (`compose` [(a, TFun t1 t2)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TFun t1 t2) xc
        (TVar a, TFun t1 t2) ->
          (`compose` [(a, TFun t1 t2)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TFun t1 t2) xc
        (TTuple t1, TVar a) ->
          (`compose` [(a, TTuple t1)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TTuple t1) xc
        (TVar a, TTuple t1) ->
          (`compose` [(a, TTuple t1)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TTuple t1) xc
        (TList t1, TVar a) ->
          (`compose` [(a, TList t1)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TList t1) xc
        (TVar a, TList t1) ->
          (`compose` [(a, TList t1)]) <$> tyUnify newc
            where newc = replaceFvInCons a (TList t1) xc

        _ -> Left $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)

typeCheck :: Monad m => Command -> ExceptT String (IStateT m) (Ty, Subst)
typeCheck cmd = do
  st <- lift $ get
  case cmd of
    CExpr e -> do
      let (r, n') = runState (genConst (tyenv st) e) (freshId st)
      (ts, const) <- except r
      s <- except $ tyUnify const
      lift $ put $ st { freshId = n' }
      return (tySubst s ts, s)
    CDecl d -> do
      let (r, n') = runState (genConstDecl (tyenv st) d) (freshId st)
      (ts, const, _) <- except r
      sigma <- except $ tyUnify const
      let ts' = tySchemaSubst sigma ts
      put $ st { freshId = n', tyenv = (nameOfDecl d, ts') : (tyenv st) }
      return (snd ts', sigma)
  `catchE`
    \s -> throwE ("Type error: " ++ s)
