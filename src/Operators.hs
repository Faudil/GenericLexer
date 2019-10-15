module Operator where

import Control.Monad
import Control.Applicative
import Data.Char
import Text.Read
import Lexer


oneOf :: [Char] -> StreamLexer Char
oneOf s = satisfy (flip elem s)

chainl :: StreamLexer a -> StreamLexer (a -> a -> a) -> a -> StreamLexer a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: StreamLexer a -> StreamLexer (a -> a -> a) -> StreamLexer a
term `chainl1` op = do {a <- term; rest a}
  where rest a = (do f <- op
                     b <- term
                     rest (f a b))
                 <|> return a

chain :: StreamLexer a -> StreamLexer (a -> a -> a) -> StreamLexer a -> StreamLexer a
chain term op end = do {a <- term; f <- op; rest a}
  where rest a = (do b <- term
                     f <- op
                     rest (f a b))
                 <|> return a

char :: Char -> StreamLexer Char
char c = satisfy (c ==)

natural :: StreamLexer Integer
natural = read <$> some (satisfy isDigit)

string :: String -> StreamLexer String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

token :: StreamLexer a -> StreamLexer a
token p = do
            a <- p
            spaces
            return a

reserved :: String -> StreamLexer String
reserved s = token (string s)

spaces :: StreamLexer String
spaces = many $ oneOf " \n\r"

digit :: StreamLexer Char
digit = satisfy isDigit

alphaNum :: StreamLexer Char
alphaNum = satisfy isAlphaNum

number :: StreamLexer Int
number = do
  spaces
  x <- string "-" <|> return []
  xs <- some digit
  spaces
  return $ read (x ++ xs)

floatingNumber :: StreamLexer Double
floatingNumber = do spaces
                    b <- string "-" <|> return []
                    m <- some digit
                    p <- string "."
                    e <- some digit
                    spaces
                    return $ read (b ++ m ++ p ++ e)

bool :: StreamLexer Bool
bool = do spaces
          v <- reserved "True" <|> reserved "False"
          return $ v == "True"

word :: StreamLexer String
word = do
       w <- some alphaNum
       spaces
       return w

anyChar :: StreamLexer Char
anyChar = satisfy (\_ -> True)

innerString :: StreamLexer String
innerString = do
       w <- some $ satisfy (\x -> x /= '"')
       return w

sepOp :: String -> (a -> a -> a) -> StreamLexer (a -> a -> a)
sepOp x p = reserved x >> return p

bracket :: StreamLexer a -> StreamLexer a
bracket rule = do
              reserved "{"
              n <- rule
              reserved "}"
              return n

quote :: StreamLexer a -> StreamLexer a
quote rule = do
              reserved "\""
              n <- rule
              reserved "\""
              return n

simpleQuote :: StreamLexer a -> StreamLexer a
simpleQuote rule = do
            reserved "'"
            n <- rule
            reserved "'"
            return n


sqBracket :: StreamLexer a -> StreamLexer a
sqBracket rule = do
  reserved "["
  n <- rule
  reserved "]"
  return n


parenthesis :: StreamLexer a -> StreamLexer a
parenthesis rule = do
  reserved "("
  n <- rule
  reserved ")"
  return n

bindStr :: String -> (a -> a -> a) -> StreamLexer (a -> a -> a)
bindStr toBind fun = reserved toBind >> return fun