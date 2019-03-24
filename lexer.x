{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
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
  \*                             { lex' TokenTimes       }
  \/                             { lex' TokenDiv         }
  \(                             { lex' TokenLParen      }
  \)                             { lex' TokenRParen      }
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
  | TokenTimes
  | TokenDiv
  | TokenLParen
  | TokenRParen
  | TokenSemiSemi
  | TokenEOF
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex TokenLet = "let"
unLex TokenRec = "rec"
unLex TokenIn = "in"
unLex TokenIf = "if"
unLex TokenThen = "then"
unLex TokenElse = "else"
unLex TokenFun = "fun"
unLex TokenArrow = "->"
unLex (TokenInt i) = show i
unLex (TokenVar s) = show s
unLex TokenEq = "="
unLex TokenLT = "<"
unLex TokenGT = ">"
unLex TokenLE = "<="
unLex TokenGE = ">="
unLex TokenAndAnd = "&&"
unLex TokenOrOr = "||"
unLex TokenPlus = "+"
unLex TokenMinus = "-"
unLex TokenTimes = "*"
unLex TokenDiv = "/"
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenEOF = "<EOF>"
unLex TokenSemiSemi = ";;"

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
