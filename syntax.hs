module Syntax where

data Command = CExpr Expr
             | CDecl Decl
             deriving (Show)

data Decl = DLet String Expr
          | DLetRec String Expr
    deriving (Show)

data Expr = EConstInt Integer
          | EConstBool Bool
          | EVar String
          | ENot Expr
          | ENeg Expr
          | EBinop Binop Expr Expr
          | EIf Expr Expr Expr
          | ELet String Expr Expr
          | EFun String Expr
          -- | ELetRec String [String] Expr Expr
          | EApp Expr Expr
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
           | VFun String Expr Env
           deriving (Show)

type Env = [(String, Value)]

valToStr :: Value -> String
valToStr v =
    case v of
      VInt n -> show n
      VBool b -> show b
      VFun {} -> "<fun>"

nameOfDecl :: Decl -> String
nameOfDecl d =
    case d of
      DLet x _ -> x
      DLetRec x _ -> x
