module Syntax where

data Expr =
        EConstInt Integer
      | EConstBool Bool
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
      deriving (Show)

data Value =
        VInt Integer
      | VBool Bool
      deriving (Show, Eq, Ord)
