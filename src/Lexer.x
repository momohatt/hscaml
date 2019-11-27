{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                        ;
  let                            { lex' TokenLet         }
  rec                            { lex' TokenRec         }
  in                             { lex' TokenIn          }
  if                             { lex' TokenIf          }
  then                           { lex' TokenThen        }
  else                           { lex' TokenElse        }
  fun                            { lex' TokenFun         }
  \-\>                           { lex' TokenArrow       }
  match                          { lex' TokenMatch       }
  with                           { lex' TokenWith        }
  true                           { lex' TokenTrue        }
  false                          { lex' TokenFalse       }
  $digit+                        { lex (TokenInt . read) }
  $alpha [$alpha $digit \_ \']*  { lex  TokenVar         }
  \=                             { lex' TokenEq          }
  \<                             { lex' TokenLT          }
  \>                             { lex' TokenGT          }
  \<\=                           { lex' TokenLE          }
  \>\=                           { lex' TokenGE          }
  \&\&                           { lex' TokenAndAnd      }
  \|\|                           { lex' TokenOrOr        }
  \+                             { lex' TokenPlus        }
  \-                             { lex' TokenMinus       }
  \*                             { lex' TokenAsterisk    }
  \/                             { lex' TokenDiv         }
  \(                             { lex' TokenLParen      }
  \)                             { lex' TokenRParen      }
  \[                             { lex' TokenLBracket    }
  \]                             { lex' TokenRBracket    }
  \,                             { lex' TokenComma       }
  \;                             { lex' TokenSemi        }
  \:\:                           { lex' TokenColonColon  }
  \|                             { lex' TokenBar         }
  \;\;                           { lex' TokenSemiSemi    }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenLet
  | TokenRec
  | TokenIn
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFun
  | TokenArrow
  | TokenMatch
  | TokenWith
  | TokenTrue
  | TokenFalse
  | TokenInt Integer
  | TokenVar String
  | TokenEq
  | TokenLT
  | TokenGT
  | TokenLE
  | TokenGE
  | TokenAndAnd
  | TokenOrOr
  | TokenPlus
  | TokenMinus
  | TokenAsterisk
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenComma
  | TokenSemi
  | TokenColonColon
  | TokenBar
  | TokenSemiSemi
  | TokenEOF

-- For nice parser error messages.
instance Show TokenClass where
  show TokenLet = "let"
  show TokenRec = "rec"
  show TokenIn = "in"
  show TokenIf = "if"
  show TokenThen = "then"
  show TokenElse = "else"
  show TokenFun = "fun"
  show TokenArrow = "->"
  show TokenMatch = "match"
  show TokenWith = "with"
  show TokenTrue = "true"
  show TokenFalse = "false"
  show (TokenInt i) = show i
  show (TokenVar s) = show s
  show TokenEq = "="
  show TokenLT = "<"
  show TokenGT = ">"
  show TokenLE = "<="
  show TokenGE = ">="
  show TokenAndAnd = "&&"
  show TokenOrOr = "||"
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenAsterisk = "*"
  show TokenDiv = "/"
  show TokenLParen = "("
  show TokenRParen = ")"
  show TokenLBracket = "["
  show TokenRBracket = "]"
  show TokenComma = ","
  show TokenEOF = "<EOF>"
  show TokenSemi = ";"
  show TokenColonColon = "::"
  show TokenBar = "|"
  show TokenSemiSemi = ";;"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> String -> Either String a
runAlex' a input = runAlex input (setFilePath "<stdin>" >> a)
}
