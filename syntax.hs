module Syntax
    ( Command(..)
    , Decl(..)
    , Expr(..)
    , Pattern(..)
    , Binop(..)
    , Value(..)
    , Env
    , valToStr
    , nameOfDecl
    ) where

data Command = CExpr Expr
             | CDecl Decl
             deriving (Show)

data Decl = DLet String Expr
          | DLetRec String Expr
    deriving (Show)

data Expr = EConstInt Integer
          | EConstBool Bool
          | EVar String
          | ETuple [Expr]
          | ENil
          | ECons Expr Expr
          | ENot Expr
          | ENeg Expr
          | EBinop Binop Expr Expr
          | EIf Expr Expr Expr
          | ELetIn Decl Expr
          | EFun String Expr
          | EApp Expr Expr
          | EMatch Expr [(Pattern, Expr)]
          deriving (Show)

data Pattern = PInt Integer
             | PBool Bool
             | PVar String
             | PTuple [Pattern]
             | PNil
             | PCons Pattern Pattern
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
           | VTuple [Value]
           | VNil
           | VCons Value Value
           deriving (Show)

type Env = [(String, Value)]

valToStr :: Value -> String
valToStr v =
    case v of
      VInt n -> show n
      VBool True -> "true"
      VBool False -> "false"
      VFun {} -> "<fun>"
      VTuple vs -> "(" ++ valToStr (head vs) ++ concatMap (\v -> ", " ++ valToStr v) (tail vs) ++ ")"
      VNil -> "[]"
      VCons v1 v2 -> "[" ++ valToStr v1 ++ listToStr' v2
        where listToStr' v = case v of
                               VNil -> "]"
                               VCons v1 v2 -> "; " ++ valToStr v1 ++ listToStr' v2

nameOfDecl :: Decl -> String
nameOfDecl d =
    case d of
      DLet x _ -> x
      DLetRec x _ -> x
