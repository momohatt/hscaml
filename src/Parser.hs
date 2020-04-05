module Parser
  ( parseCmd
  ) where

import           Control.Applicative        (pure, (*>), (<$>), (<$), (<*), (<*>))
import           Control.Monad.Combinators.Expr
import           Data.Functor               (($>))
import           Data.Maybe                 (isJust)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Syntax

type Parser = Parsec Void String

parseCmd :: String -> Either String Command
parseCmd input =
  case parse (sc >> command <* symbol ";;") "" input of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right c  -> Right c

command :: Parser Command
command = toplevelLet
      <|> CExpr <$> expr
      <?> "command"

-- Parser after "let"
decl :: Parser Decl
decl = do
  isRec <- isJust <$> optional (reserved "rec")
  var   <- lowerId
  args  <- many lowerId
  body  <- symbol "=" >> expr
  return $ if isRec then DLetRec var (foldr EFun body args)
                    else DLet    var (foldr EFun body args)

toplevelLet :: Parser Command
toplevelLet = do
  bind <- reserved "let" >> decl
  body <- optional (reserved "in" >> expr)
  case body of
    Nothing -> return $ CDecl bind
    Just b' -> return $ CExpr (ELetIn bind b')

expr :: Parser Expr
expr = EIf    <$> (reserved "if" >> expr) <*> (reserved "then" >> expr)
                                          <*> (reserved "else" >> expr)
   <|> ELetIn <$> (reserved "let" >> decl)    <*> (reserved "in" >> expr)
   <|> EFun   <$> (reserved "fun" >> lowerId) <*> (symbol "->" >> expr)
   <|> EMatch <$> (reserved "match" >> expr)  <*> (reserved "with" >> matchClause1)
   <|> opExpr

matchClause1 :: Parser [(Pattern, Expr)]
matchClause1 = do
  first <- optional (symbol "|") >> matchClause
  rest  <- many (symbol "|" >> matchClause)
  return (first : rest)
  where
    matchClause :: Parser (Pattern, Expr)
    matchClause = (,) <$> pattern <*> (symbol "->" >> expr)

opExpr :: Parser Expr
opExpr = makeExprParser atomExpr table
  where
    table :: [[Operator Parser Expr]]
    table =
      [ [ InfixL (EApp        <$ sc         ) ]
      , [ Prefix (ENeg        <$ symbol "-" ) ]
      , [ InfixL (EBinop BMul <$ symbol "*" )
        , InfixL (EBinop BDiv <$ symbol "/" ) ]
      , [ InfixL (EBinop BAdd <$ symbol "+" )
        , InfixL (EBinop BSub <$ symbol "-" ) ]
      , [ InfixR (ECons       <$ symbol "::") ]
      , [ InfixL (EBinop BEq  <$ symbol "=" ) ]
      , [ InfixL (EBinop BGE  <$ symbol ">=")
        , InfixL (EBinop BLE  <$ symbol "<=")
        , InfixL (EBinop BGT  <$ symbol ">" )
        , InfixL (EBinop BLT  <$ symbol "<" ) ]
      , [ InfixL (EBinop BAnd <$ symbol "&&") ]
      , [ InfixL (EBinop BOr  <$ symbol "||") ]
      ]

parenExpr :: Parser Expr
parenExpr = do
  elems <- parens $ sepBy expr comma
  case elems of
    [x] -> return x
    _   -> return $ ETuple elems

bracketExpr :: Parser Expr
bracketExpr = do
  elems <- brackets $ sepBy expr (symbol ";")
  case elems of
    [] -> return ENil
    _  -> return $ foldr ECons ENil elems

atomExpr :: Parser Expr
atomExpr = EConstInt  <$> positiveIntegerLiteral
       <|> EConstBool <$> boolLiteral
       <|> EVar       <$> lowerId
       <|> parenExpr
       <|> bracketExpr
       <?> "atomic expression"

pattern :: Parser Pattern
pattern = opPattern
      <?> "pattern"

opPattern :: Parser Pattern
opPattern = makeExprParser atomPattern table
  where
    table :: [[Operator Parser Pattern]]
    table =
      [ [ InfixR (PCons <$ symbol "::" ) ]
      ]

parenPattern :: Parser Pattern
parenPattern = do
  elems <- parens $ sepBy pattern comma
  case elems of
    [x] -> return x
    _   -> return $ PTuple elems

bracketPattern :: Parser Pattern
bracketPattern = do
  elems <- brackets $ sepBy pattern comma
  case elems of
    []  -> return PNil
    _   -> return $ foldr PCons PNil elems

atomPattern :: Parser Pattern
atomPattern = PInt  <$> positiveIntegerLiteral
          <|> PInt . negate <$> (symbol "-" >> positiveIntegerLiteral)
          <|> PBool <$> boolLiteral
          <|> PVar  <$> lowerId
          <|> parenPattern
          <|> bracketPattern
          <?> "atomic pattern"

--
-- Tokens
--

-- Space Comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockCommentNested "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = lexeme L.decimal
                     <?> "unsinged integer"

boolLiteral :: Parser Bool
boolLiteral = reserved "true"  $> True
          <|> reserved "false" $> False
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
