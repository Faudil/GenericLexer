module Main where

import System.Environment

import Lexer
import Operators

-- This is an example for a grammar for an eval-expr
data Expr =
  ExprAdd Expr Expr
  | ExprSub Expr Expr
  | ExprMul Expr Expr
  | ExprDiv Expr Expr
  | ExprDouble Double
  deriving (Show, Eq, Ord)

-- Here we define what is a number and parse it using a function floatingNumber of Operators
primary :: StreamLexer Expr
primary = do n <- floatingNumber
             return $ ExprDouble n

expression = parenthesis grammar <|> primary

-- Now we define the strings associated with each operator
sumOp = (sepOp "+" ExprAdd) <|> (sepOp "-" ExprSub)
coeffOp = (sepOp "*" ExprMul) <|> (sepOp "/" ExprDiv)


fact = expression `chainl1` coeffOp

grammar :: StreamLexer Expr
grammar = fact `chainl1` sumOp


parse :: String -> Expr
parse = genTree grammar


main :: IO ()
main = do x <- getArgs
          print $ x !! 0
          print $ parse $ x !! 0
          return ()



