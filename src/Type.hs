module Type
  ( Ty(..)
  , Constraint
  , TyEnv
  , Subst
  , TySchema
  ) where

data Ty = TInt
        | TBool
        | TFun Ty Ty
        | TTuple [Ty]
        | TList Ty
        | TVar String

type Constraint = [(Ty, Ty)]
type TyEnv = [(String, TySchema)]
type Subst = [(String, Ty)]
type TySchema = ([String], Ty)

instance Show Ty where
  show TInt = "int"
  show TBool = "bool"
  show (TFun ts t) = show ts ++ " -> " ++ show t
  show (TTuple ts) = "(" ++ show (head ts) ++ concatMap (\t -> " * " ++ show t) (tail ts) ++ ")"
  show (TList t) = show t ++ " list"
  show (TVar a) = a
