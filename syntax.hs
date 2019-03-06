module Syntax where

data Expr =
        EConstInt Integer
      | EConstBool Bool
      | ENeg Expr
      | EAdd Expr Expr
      | ESub Expr Expr
      | EMul Expr Expr
      | EDiv Expr Expr
      deriving (Show)

data Value =
        VInt Int
      | VBool Bool
      deriving (Show)
