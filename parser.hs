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
                                     , "fun"
                                     , ";;"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                     , "&&", "||", "not"
                                     , "->"
                                     , "(", ",", ")"
                                     , "<", ">", "<=", ">="
                                     ]
           , Token.caseSensitive    = True
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
natural    = Token.natural    lexer
whiteSpace = Token.whiteSpace lexer

parser :: Parser Command
parser = whiteSpace >> command

command :: Parser Command
command =  try (CExpr <$> expr <* reserved ";;")  -- need 'try' for letDecl and letExpr
       <|> (CDecl <$> decl <* reserved ";;")

decl :: Parser Decl
decl =  try letDecl
    <|> letRecDecl

letDecl = try (DLet <$> (reserved "let" *> identifier) <*> (reserved "=" *> expr))
      <|> DLet <$> (reserved "let" *> identifier) <*> funExpr

letRecDecl = DLetRec <$> (reserved "let" *> reserved "rec" *> identifier) <*> funExpr

funExpr = try (EFun <$> (identifier <* reservedOp "=") <*> expr)
      <|> EFun <$> identifier <*> funExpr

absFunExpr = try (EFun <$> (reserved "fun" *> identifier) <*> (reservedOp "->" *> expr))
         <|> EFun <$> (reserved "fun" *> identifier) <*> absFunExpr'

absFunExpr' = try (EFun <$> identifier <*> (reservedOp "->" *> expr))
          <|> EFun <$> identifier <*> absFunExpr'

expr :: Parser Expr
expr =  try ifExpr
    <|> try letExpr
    <|> try letRecExpr
    <|> try absFunExpr
    <|> buildExpressionParser ops term

ifExpr = EIf <$>
        (reserved "if" *> expr) <*>
            (reserved "then" *> expr) <*>
                (reserved "else" *> expr)

letExpr = try (ELet <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expr) <*> (reserved "in" *> expr))
      <|> ELet <$> (reserved "let" *> identifier) <*> funExpr <*> (reserved "in" *> expr)

letRecExpr =
    ELetRec <$> (reserved "let" *> reserved "rec" *> identifier) <*>
        funExpr <*>
            (reserved "in" *> expr)

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

term = try appExpr
    <|> atom

appExpr =
    (\l -> if length l == 1 then head l else foldl1 EApp l) <$> many1 atom

atom =  try ((\x y -> ETuple $ x : y) <$> (reservedOp "(" *> expr) <*> many1 (reservedOp "," *> expr) <* reservedOp ")")
    <|> parens expr
    <|> EVar <$> identifier
    <|> EConstInt <$> natural
    <|> (reserved "true"  >> return (EConstBool True ))
    <|> (reserved "false" >> return (EConstBool False))

parseString :: String -> Command
parseString str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r
