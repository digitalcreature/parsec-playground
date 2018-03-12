module Regex where

import Control.Monad
import Data.Char

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char

type Parser = Parsec String ()
  
data AST
  = Seq [AST]
  | Many AST
  | Many1 AST
  deriving Show


rexpr = liftM Seq (sepBy postfixed (char '|'))

postfixed = buildExpressionParser table term
  where table = [
            [ Postfix ((char '+') *> return Many1), Postfix ((char '*') *> return Many) ]
          ]

term :: Parser AST
term = rexpr
