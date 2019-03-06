module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Syntax

languageDef =
  emptyDef { Token.commentStart    = "(*"
           , Token.commentEnd      = "*)"
           , Token.nestedComments  = True
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "true"
                                     , "false"
                                     , "let"
                                     , "rec"
                                     , "in"
                                     , ";;"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                     , "&&", "||", "not"
                                     , "<", ">", "<=", ">="
                                     ]
           , Token.caseSensitive    = True
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer

parser :: Parser Command
parser = whiteSpace >> command

command :: Parser Command
command =  try (CExpr <$> expr <* reserved ";;")  -- need 'try' for letDecl and letExpr
       <|> (CDecl <$> decl <* reserved ";;")

decl :: Parser Decl
decl =  try letDecl
    <|> letRecDecl

letDecl =
    DLet <$>
        (reserved "let" *> identifier) <*>
            (reserved "=" *> expr)

letRecDecl =
    DLetRec <$>
        (reserved "let" *> reserved "rec" *> identifier) <*>
            many1 identifier <*>
                (reservedOp "=" *> expr)

expr :: Parser Expr
expr =  try ifExpr
    <|> try letExpr
    <|> try letRecExpr
    <|> try appExpr
    <|> buildExpressionParser ops term

ifExpr =
    EIf <$>
        (reserved "if" *> expr) <*>
            (reserved "then" *> expr) <*>
                (reserved "else" *> expr)

letExpr =
    ELet <$>
        (reserved "let" *> identifier) <*>
            (reservedOp "=" *> expr) <*>
                (reserved "in" *> expr)

letRecExpr =
    ELetRec <$>
        (reserved "let" *> reserved "rec" *> identifier) <*>
            many1 identifier <*>
                (reservedOp "=" *> expr) <*>
                    (reserved "in" *> expr)

appExpr =
    EApp <$> identifier <*> many1 term

ops = [ [Prefix (reservedOp "-"  >> return ENeg)          ]
      , [Infix  (reservedOp "*"  >> return (EBinop BMul)) AssocLeft,
         Infix  (reservedOp "/"  >> return (EBinop BDiv)) AssocLeft,
         Infix  (reservedOp "&&" >> return (EBinop BAnd)) AssocLeft]
      , [Infix  (reservedOp "+"  >> return (EBinop BAdd)) AssocLeft,
         Infix  (reservedOp "-"  >> return (EBinop BSub)) AssocLeft,
         Infix  (reservedOp "||" >> return (EBinop BOr )) AssocLeft]
      , [Infix  (reservedOp "="  >> return (EBinop BEq )) AssocLeft,
         Infix  (reservedOp ">"  >> return (EBinop BGT )) AssocLeft,
         Infix  (reservedOp "<"  >> return (EBinop BLT )) AssocLeft,
         Infix  (reservedOp ">=" >> return (EBinop BGE )) AssocLeft,
         Infix  (reservedOp "<=" >> return (EBinop BLE )) AssocLeft]
       ]

term =  parens expr
    <|> EVar <$> identifier
    <|> EConstInt <$> integer
    <|> (reserved "true"  >> return (EConstBool True ))
    <|> (reserved "false" >> return (EConstBool False))

parseString :: String -> Command
parseString str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r
