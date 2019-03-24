{
{-# OPTIONS -w #-}
module Parser( parseExpr ) where

import Syntax
import Lexer

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%right let in
%right if then else
%right fun arrow
%left oror
%left andand
%left '<' '>' '=' le ge
%left '+' '-'
%left '*' '/'

%token
      let             { Token _ TokenLet }
      rec             { Token _ TokenRec }
      in              { Token _ TokenIn }
      if              { Token _ TokenIf }
      then            { Token _ TokenThen }
      else            { Token _ TokenElse }
      fun             { Token _ TokenFun }
      arrow           { Token _ TokenArrow }
      int             { Token _ (TokenInt $$) }
      var             { Token _ (TokenVar $$) }
      semisemi        { Token _ TokenSemiSemi }
      '='             { Token _ TokenEq }
      '<'             { Token _ TokenLT }
      '>'             { Token _ TokenGT }
      le              { Token _ TokenLE }
      ge              { Token _ TokenGE }
      andand          { Token _ TokenAndAnd }
      oror            { Token _ TokenOrOr }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%

Command :
    Expr semisemi   { CExpr $1 }
  | Decl semisemi   { CDecl $1 }

Decl :
    let     var      '=' Expr  { DLet $2 $4 }
  | let     var Args '=' Expr  { DLet $2 ($3 $5) }
  | let rec var      '=' Expr  { DLetRec $3 $5 }
  | let rec var Args '=' Expr  { DLetRec $3 ($4 $6) }

Args :
    var         { \x -> EFun $1 x }
  | Args var    { \x -> $1 (EFun $2 x) }

Expr :
    Decl in Expr                { ELetIn $1 $3 }
  | if Expr then Expr else Expr { EIf $2 $4 $6 }
  | fun Args arrow Expr         { $2 $4 }
  | Expr '+'    Expr            { EBinop BAdd $1 $3 }
  | Expr '-'    Expr            { EBinop BSub $1 $3 }
  | Expr '*'    Expr            { EBinop BMul $1 $3 }
  | Expr '/'    Expr            { EBinop BDiv $1 $3 }
  | Expr '='    Expr            { EBinop BEq  $1 $3 }
  | Expr '<'    Expr            { EBinop BLT  $1 $3 }
  | Expr '>'    Expr            { EBinop BGT  $1 $3 }
  | Expr le     Expr            { EBinop BLE  $1 $3 }
  | Expr ge     Expr            { EBinop BGE  $1 $3 }
  | Expr andand Expr            { EBinop BAnd $1 $3 }
  | Expr oror   Expr            { EBinop BOr  $1 $3 }
  | AppExpr                     { $1 }

AppExpr :
    AppExpr Atom { EApp $1 $2 }
  | Atom         { $1 }

Atom :
    int           { EConstInt $1 }
  | var           { EVar $1 }
  | '(' Expr ')'  { $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExpr :: String -> Either String Command
parseExpr = runAlex' parse
}
