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

%left oror
%left andand
%left '<' '>' '=' le ge
%left '+' '-'
%left '*' '/'

%token
      let             { Token _ TokenLet }
      in              { Token _ TokenIn }
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

Command : Expr semisemi              { CExpr $1 }

Expr : Expr '+'    Expr { EBinop BAdd $1 $3 }
     | Expr '-'    Expr { EBinop BSub $1 $3 }
     | Expr '*'    Expr { EBinop BMul $1 $3 }
     | Expr '/'    Expr { EBinop BDiv $1 $3 }
     | Expr '='    Expr { EBinop BEq  $1 $3 }
     | Expr '<'    Expr { EBinop BLT  $1 $3 }
     | Expr '>'    Expr { EBinop BGT  $1 $3 }
     | Expr le     Expr { EBinop BLE  $1 $3 }
     | Expr ge     Expr { EBinop BGE  $1 $3 }
     | Expr andand Expr { EBinop BAnd $1 $3 }
     | Expr oror   Expr { EBinop BOr  $1 $3 }
     | Atom             { $1 }

Atom : int           { EConstInt $1 }
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
