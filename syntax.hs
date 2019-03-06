module Syntax where

data Command = CExpr Expr
             | CDecl Decl
             deriving (Show)

data Decl = DLet String Expr
          | DLetRec String [String] Expr
    deriving (Show)

data Expr = EConstInt Integer
          | EConstBool Bool
          | EVar String
          | ENot Expr
          | ENeg Expr
          | EBinop Binop Expr Expr
          | EIf Expr Expr Expr
          | ELet String Expr Expr
          | ELetRec String [String] Expr Expr
          | EApp String [Expr]
          deriving (Show)

data Binop = BAnd
           | BOr
           | BAdd
           | BSub
           | BMul
           | BDiv
           | BEq
           | BGT
           | BLT
           | BGE
           | BLE
           deriving (Show, Eq)

data Value = VInt Integer
           | VBool Bool
           | VFun String [String] Env Expr
           deriving (Show)

type Env = [(String, Value)]
