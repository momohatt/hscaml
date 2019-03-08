{-# LANGUAGE TupleSections #-}

module Typing where

import Control.Monad.State
import Data.Maybe
import Data.Either

import Syntax
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

generalize :: TyEnv -> Ty -> TySchema
generalize env t =
    (filter (`notElem` l2) l1, t)
        where l1 = getTyVars t
              l2 = concatMap (getTyVars . snd . snd) env

instantiate :: TySchema -> State Int Ty
instantiate (l, t) = instantiate' l t

instantiate' :: [String] -> Ty -> State Int Ty
instantiate' l t =
    case l of
      [] -> return t
      x : xl -> do
          y <- genNewTyVar
          instantiate' xl (tySubst [(x, y)] t)

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
                  Just t -> Right . (, []) <$> instantiate t -- TODO: pattern match fail
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
      ELet x e1 e2 -> do
          r1 <- genConstDecl env (DLet x e1)
          case r1 of
            Left msg -> return $ Left msg
            Right (t1, c1) -> do
                -- t1 <- instantiate ts1
                r2 <- genConst ((x, t1) : env) e2
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

genConstDecl :: TyEnv -> Decl -> State Int (Either String (TySchema, Constraint))
genConstDecl env d =
    case d of
      DLet x e -> do
          r <- genConst env e
          case r of
            Right (t, c) ->
                case tyUnify c of
                  Right sigma -> do
                      let t' = generalize env $ tySubst sigma t
                      return $ Right (t', (t, snd t') : c)
                  Left msg -> return $ Left msg
            Left msg -> return $ Left msg
      DLetRec f e -> do
          t <- genNewTyVar
          r <- genConst ((f, ([], t)) : env) e
          case r of
            Right (t, c) ->
                case tyUnify c of
                  Right sigma -> do
                      let t' = generalize env $ tySubst sigma t
                      return $ Right (t', (t, snd t') : c)
                  Left msg -> return $ Left msg
            Left msg -> return $ Left msg

tySubst :: Subst -> Ty -> Ty
tySubst s t =
    case t of
      TFun ts t ->
          TFun (tySubst s ts) (tySubst s t)
      TVar x -> fromMaybe (TVar x) $ lookup x s
      _ -> t

compose :: Subst -> Subst -> Subst
compose s1 s2 =
    s1 ++ map (\(v, t) -> (v, tySubst s1 t)) s2

checkFv :: Ty -> String -> Bool
checkFv t v =
    case t of
      TInt -> False
      TBool -> False
      TFun a b -> checkFv a v || checkFv b v
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
            (TVar x1, TVar x2)
              | x1 == x2 -> tyUnify xc
              | otherwise -> ((x1, TVar x2) :) <$> tyUnify xc
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
            (TFun t1 t2, TVar a) ->
                case checkFv (TFun t1 t2) a of
                  True -> Left $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TFun t1 t2)]) <$> tyUnify newc
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            (TVar a, TFun t1 t2) ->
                case checkFv (TFun t1 t2) a of
                  True -> Left $"cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)
                  False -> (`compose` [(a, TFun t1 t2)]) <$> tyUnify newc
                               where newc = replaceFvInCons a (TFun t1 t2) xc
            _ -> Left $ "cannot unify " ++ show (fst ts) ++ " and " ++ show (snd ts)

typeCheck :: Int -> TyEnv -> Command -> Either String (Ty, TyEnv, Int)
typeCheck n tenv c =
    case c of
      CExpr e ->
          case r of
            Left msg -> Left msg
            Right (ts, const) ->
                (\s -> (tySubst s ts, tenv, n')) <$> tyUnify const
          where (r, n') = runState (genConst tenv e) n
      CDecl d ->
          case r of
            Left msg -> Left msg
            Right (ts, const) ->
                (\sigma ->
                    let ts' = tySchemaSubst sigma ts in
                        (snd ts', (nameOfDecl d, ts') : tenv, n')) <$> tyUnify const
          where (r, n') = runState (genConstDecl tenv d) n
