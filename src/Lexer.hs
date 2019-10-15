module Lexer (StreamLexer,
                satisfy, genTree, item, (<|>)) where

import Data.Char
import Control.Monad
import Control.Applicative

newtype StreamLexer a = StreamLexer { parse :: String -> [(a, String)] }

genTree :: StreamLexer a -> String -> a
genTree prs str =
        case parse prs str of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire stream."
        _           -> error "Parser error."

item :: StreamLexer Char
item = StreamLexer $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]


instance Functor StreamLexer where
  fmap f  (StreamLexer xs) = StreamLexer (\s -> [(f a, b) | (a, b) <- xs s])

instance Applicative StreamLexer where
  pure = return
  (StreamLexer xs1) <*>  (StreamLexer xs2) = StreamLexer (\s -> [(f a, s2) | (f, s1) <- xs1 s, (a, s2) <- xs2 s1])


bind :: StreamLexer a -> (a -> StreamLexer b) -> StreamLexer b
bind toParse fun = StreamLexer $ \s -> concatMap (\(a, s') -> parse (fun a) s') $ parse toParse s

unit :: a -> StreamLexer a
unit x = StreamLexer (\s -> [(x, s)])

instance Monad StreamLexer where
  return = unit
  (>>=)  = bind


instance MonadPlus StreamLexer where
  mzero = failure
  mplus = combine

instance Alternative StreamLexer where
  empty = mzero
  (<|>) = option

combine :: StreamLexer a -> StreamLexer a -> StreamLexer a
combine p q = StreamLexer (\s -> parse p s ++ parse q s)

failure :: StreamLexer a
failure = StreamLexer (\cs -> [])

option :: StreamLexer a -> StreamLexer a -> StreamLexer a
option p q = StreamLexer $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res


satisfy :: (Char -> Bool) -> StreamLexer Char
satisfy term = item `bind` \x ->
  if term x
  then unit x
  else  (StreamLexer (\xs -> []))