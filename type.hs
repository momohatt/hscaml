module Type
    ( Ty(..)
    , Constraint
    , TyEnv
    , Subst
    , TySchema
    , tyToStr
    ) where

data Ty = TInt
        | TBool
        | TFun Ty Ty
        | TTuple [Ty]
        | TVar String
        deriving (Show)

type Constraint = [(Ty, Ty)]
type TyEnv = [(String, TySchema)]
type Subst = [(String, Ty)]
type TySchema = ([String], Ty)

tyToStr :: Ty -> String
tyToStr t =
    case t of
      TInt -> "int"
      TBool -> "bool"
      TFun ts t -> tyToStr ts ++ " -> " ++ tyToStr t
      TVar a -> a
      TTuple ts -> "(" ++ tyToStr (head ts) ++ concatMap (\t -> " * " ++ tyToStr t) (tail ts) ++ ")"
