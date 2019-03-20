{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Type
    ( Ty(..)
    , Constraint
    , TyEnv
    , Subst
    , TySchema
    , tyToStr
    ) where

import Control.DeepSeq
import GHC.Generics (Generic)

data Ty = TInt
        | TBool
        | TFun Ty Ty
        | TTuple [Ty]
        | TList Ty
        | TVar String
        deriving (Show, Generic, NFData)

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
      TTuple ts -> "(" ++ tyToStr (head ts) ++ concatMap (\t -> " * " ++ tyToStr t) (tail ts) ++ ")"
      TList t -> tyToStr t ++ " list"
      TVar a -> a
