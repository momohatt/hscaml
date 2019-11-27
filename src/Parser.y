{
{-# OPTIONS -w #-}
module Parser( parseExpr ) where

import Syntax
import Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%right let in
%right if then else
%right fun "->"
%left "||"
%left "&&"
%left '<' '>' '=' "<=" ">="
%right "::"
%left '+' '-'
%left '*' '/'
%left '|'

%token
      let      { Token _ TokenLet }
      rec      { Token _ TokenRec }
      in       { Token _ TokenIn }
      if       { Token _ TokenIf }
      then     { Token _ TokenThen }
      else     { Token _ TokenElse }
      fun      { Token _ TokenFun }
      "->"     { Token _ TokenArrow }
      match    { Token _ TokenMatch }
      with     { Token _ TokenWith }
      true     { Token _ TokenTrue }
      false    { Token _ TokenFalse }
      int      { Token _ (TokenInt $$) }
      var      { Token _ (TokenVar $$) }
      ";;"     { Token _ TokenSemiSemi }
      '='      { Token _ TokenEq }
      '<'      { Token _ TokenLT }
      '>'      { Token _ TokenGT }
      "<="     { Token _ TokenLE }
      ">="     { Token _ TokenGE }
      "&&"     { Token _ TokenAndAnd }
      "||"     { Token _ TokenOrOr }
      '+'      { Token _ TokenPlus }
      '-'      { Token _ TokenMinus }
      '*'      { Token _ TokenAsterisk }
      '/'      { Token _ TokenDiv }
      '('      { Token _ TokenLParen }
      ')'      { Token _ TokenRParen }
      '['      { Token _ TokenLBracket }
      ']'      { Token _ TokenRBracket }
      "::"     { Token _ TokenColonColon }
      ';'      { Token _ TokenSemi }
      ','      { Token _ TokenComma }
      '|'      { Token _ TokenBar }

%%

Command :
    Expr ";;"  { CExpr $1 }
  | Decl ";;"  { CDecl $1 }

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
  | fun Args "->" Expr          { $2 $4 }
  | match Expr with Cases       { EMatch $2 $4 }
  | match Expr with '|' Cases   { EMatch $2 $5 }
  | Expr "::" Expr              { ECons $1 $3 }
  | Expr '+'  Expr              { EBinop BAdd $1 $3 }
  | Expr '-'  Expr              { EBinop BSub $1 $3 }
  | Expr '*'  Expr              { EBinop BMul $1 $3 }
  | Expr '/'  Expr              { EBinop BDiv $1 $3 }
  | Expr '='  Expr              { EBinop BEq  $1 $3 }
  | Expr '<'  Expr              { EBinop BLT  $1 $3 }
  | Expr '>'  Expr              { EBinop BGT  $1 $3 }
  | Expr "<=" Expr              { EBinop BLE  $1 $3 }
  | Expr ">=" Expr              { EBinop BGE  $1 $3 }
  | Expr "&&" Expr              { EBinop BAnd $1 $3 }
  | Expr "||" Expr              { EBinop BOr  $1 $3 }
  | AppExpr                     { $1 }

Cases :
    Pattern "->" Expr           { [($1, $3)] }
  | Pattern "->" Expr '|' Cases { ($1, $3) : $5 }

Pattern :
    PatternAtom "::" Pattern { PCons $1 $3 }
  | PatternAtom              { $1 }

PatternAtom :
    int                        { PInt $1 }
  | var                        { PVar $1 }
  | true                       { PBool True }
  | false                      { PBool False }
  | '[' ']'                    { PNil }
  | '(' sep2(Pattern, ',') ')' { PTuple $2 }

AppExpr :
    AppExpr Atom { EApp $1 $2 }
  | Atom         { $1 }

List1 :
    Expr ';' List1 { ECons $1 $3 }
  | Expr           { ECons $1 ENil }
  | Expr ';'       { ECons $1 ENil }

Atom :
    int                     { EConstInt $1 }
  | var                     { EVar $1 }
  | true                    { EConstBool True }
  | false                   { EConstBool False }
  | '(' sep2(Expr, ',') ')' { ETuple $2 }
  | '(' Expr ')'            { $2 }
  | '[' ']'                 { ENil }
  | '[' List1 ']'           { $2 }

--
-- Helpers (Parameterized Products)
--

sep2(p, q)    : p q sep1(p, q)     { $1 : $3 }
sep1(p, q)    : p list(snd(q, p))  { $1 : $2 }
sep(p, q)     : sep1(p, q)         { $1 }
              |                    { [] }

snd(p, q)     : p q                { $2 }

list1(p)      : rev_list1(p)       { reverse $1 }
list(p)       : list1(p)           { $1 }
              |                    { [] }

rev_list1(p)  : p                  { [$1] }
              | rev_list1(p) p     { $2 : $1 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseExpr :: String -> Either String Command
parseExpr = runAlex' parse
}
