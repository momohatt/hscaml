module Type where

data Ty = TInt
        | TBool
        | TFun [Ty] Ty   -- arguments are uncurried
        | TVar String
        deriving (Show)

type Constraint = [(Ty, Ty)]
type TyEnv = [(String, Ty)]
