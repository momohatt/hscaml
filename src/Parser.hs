module Parser
  ( parseCmd
  ) where

import           Control.Applicative        (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Data.Functor               (($>))
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Syntax

type Parser = Parsec Void String

parseCmd :: String -> Either String Command
parseCmd input =
  case parse command "" input of
    Left err -> Left (errorBundlePretty err)
    Right c  -> Right c

command :: Parser Command
command = CDecl <$> (reserved "let" >> decl)
      <|> CExpr <$> expr
      <?> "command"

decl :: Parser Decl
decl = DLetRec <$> (try (reserved "rec") >> lowerId) <*> expr
   <|> DLet <$> lowerId <*> expr
   <?> "let declaration"

expr :: Parser Expr
expr = atomExpr

tupleOrParenExpr :: Parser Expr
tupleOrParenExpr = do
  elems <- parens $ sepBy expr comma
  case elems of
    [x] -> return x
    _   -> return $ ETuple elems

atomExpr :: Parser Expr
atomExpr = EConstInt  <$> positiveIntegerLiteral
       <|> EConstBool <$> boolLiteral
       <|> EVar       <$> lowerId
       <|> tupleOrParenExpr
       <|> ENil       <$  brackets sc
       <?> "atomic expression"

--
-- Tokens
--

-- Space Comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockCommentNested "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = lexeme L.decimal
                     <?> "unsinged integer"

boolLiteral :: Parser Bool
boolLiteral = reserved "True"  $> True
          <|> reserved "False" $> False
          <?> "boolean"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

symbol :: String -> Parser String
symbol sym = try $ L.symbol sc sym

operator :: String -> Parser String
operator sym = try $ string sym <* notFollowedBy opChar <* sc

opChar :: Parser Char
opChar = oneOf "%^&*-+\\|:<>.?/'!#@$"

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy letterChar)

lowerId :: Parser String
lowerId = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many alphaNumChar
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

lowerReservedWords :: [String]
lowerReservedWords =
  [ "true"
  , "false"
  , "if"
  , "then"
  , "else"
  , "let"
  , "rec"
  , "in"
  , "match"
  , "with"
  , "fun"
  ]
