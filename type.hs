module Type where

data Ty = TInt
        | TBool
        | TFun Ty Ty
        | TVar String
        deriving (Show)

type Constraint = [(Ty, Ty)]
type TyEnv = [(String, Ty)]
type Subst = [(String, Ty)]

tyToStr :: Ty -> String
tyToStr t =
    case t of
      TInt -> "int"
      TBool -> "bool"
      TFun ts t -> tyToStr ts ++ " -> " ++ tyToStr t
      TVar a -> a
