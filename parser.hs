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
decl =  letDecl

letDecl =
    DLet <$>
        (reserved "let" *> identifier) <*>
            (reserved "=" *> expr)

expr :: Parser Expr
expr =  buildExpressionParser ops term
    <|> ifExpr
    <|> letExpr

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

ops = [ [Prefix (reservedOp "-"   >> return ENeg)          ]
      , [Infix  (reservedOp "*"   >> return EMul) AssocLeft,
         Infix  (reservedOp "/"   >> return EDiv) AssocLeft,
         Infix  (reservedOp "&&"  >> return EAnd) AssocLeft]
      , [Infix  (reservedOp "+"   >> return EAdd) AssocLeft,
         Infix  (reservedOp "-"   >> return ESub) AssocLeft,
         Infix  (reservedOp "||"  >> return EOr)  AssocLeft]
      , [Infix  (reservedOp "="   >> return EEq)  AssocLeft,
         Infix  (reservedOp ">"   >> return EGT)  AssocLeft,
         Infix  (reservedOp "<"   >> return ELT)  AssocLeft,
         Infix  (reservedOp ">="  >> return EGE)  AssocLeft,
         Infix  (reservedOp "<="  >> return ELE)  AssocLeft]
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
