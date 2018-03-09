module Calc where

import Control.Monad
import Data.Char

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.String hiding (Parser)

type Parser = Parsec String ()
type Val = Double

symbol :: String -> Parser String
symbol s = string s <* spaces

parens = between (symbol "(") (symbol ")")

start = spaces *> expr <* eof

binop s f a = Infix ((symbol s) *> (return f)) a
binopl s f = binop s f AssocLeft
binopr s f = binop s f AssocRight
preop s f = Prefix $ (symbol s) *> (return f)
postop s f = Postfix $ (symbol s) *> (return f)

expr = buildExpressionParser table atom
  where
    table = [
        [ postop "!" dfac ],
        [ preop "-" negate, preop "+" id],
        [ binopl "*" (*), binopl "/" (/)],
        [ binopl "+" (+), binopl "-" (-)]
      ]

atom = (try $ parens expr) <|> val

dfac :: Double -> Double
dfac d = fromIntegral (fac (floor d))

fac 0 = 1
fac d
  | d > 0 = d * (fac (d - 1))

val :: Parser Val
val = liftM convert (many1 digit) <* spaces -- <?> "number"
  where
    convert s = foldl f 0 s
    f :: Val -> Char -> Val
    f v d = v * 10 + digitToVal d

eval s = case (parse start "" s ) of
  Left err -> print err
  Right a -> print a

digitToVal :: Char -> Val
digitToVal = fromIntegral . digitToInt
