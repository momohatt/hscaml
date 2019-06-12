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
  show t = case t of
    TInt -> "int"
    TBool -> "bool"
    TFun ts t -> show ts ++ " -> " ++ show t
    TTuple ts -> "(" ++ show (head ts) ++ concatMap (\t -> " * " ++ show t) (tail ts) ++ ")"
    TList t -> show t ++ " list"
    TVar a -> a
