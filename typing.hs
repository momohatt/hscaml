module Typing where

import Control.Monad.State
import Data.Maybe

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
                  Just t -> return (t, []) -- TODO: pattern match fail
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

tySubst :: Subst -> Ty -> Ty
tySubst s t =
    case t of
      TFun ts t ->
          TFun (map (tySubst s) ts) (tySubst s t)
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
      TFun a b -> any (`checkFv` v) a || checkFv b v
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
                tyUnify $ zip s1 s2 ++ [(t1, t2)] ++ xc
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

genConstDecl :: TyEnv -> Decl -> State Int (Ty, Constraint)
genConstDecl env d =
    case d of
      DLet x e -> genConst env e
      DLetRec f xs e -> do
          ts <- mapM (const genNewTyVar) xs
          tr <- genNewTyVar
          let env' = (f, TFun ts tr) : env
          (t, c) <- genConst (zip xs ts ++ env') e
          return (TFun ts tr, (tr, t) : c)

typeCheck :: TyEnv -> Command -> Either String (Ty, TyEnv)
typeCheck tenv c =
    case c of
      CExpr e ->
          case tyUnify const of
            Right s -> Right (tySubst s t, tenv)
            Left msg -> Left msg
            where (t, const) = evalState (genConst tenv e) 0
      CDecl d ->
          case tyUnify const of
            Right s -> Right (tySubst s t, (name, tySubst s t) : tenv)
            Left msg -> Left msg
            where (t, const) = evalState (genConstDecl tenv d) 0
                  name = case d of
                           DLet x _ -> x
                           DLetRec x _ _ -> x
