module Syntax where

data Expr =
        EConstInt Integer
      | EConstBool Bool
      | EVar String
      | ENot Expr
      | EAnd Expr Expr
      | EOr Expr Expr
      | ENeg Expr
      | EAdd Expr Expr
      | ESub Expr Expr
      | EMul Expr Expr
      | EDiv Expr Expr
      | EEq Expr Expr
      | EGT Expr Expr
      | ELT Expr Expr
      | EGE Expr Expr
      | ELE Expr Expr
      | EIf Expr Expr Expr
      deriving (Show)

data Value =
        VInt Integer
      | VBool Bool
      deriving (Show, Eq, Ord)

type Env = [(String, Value)]
