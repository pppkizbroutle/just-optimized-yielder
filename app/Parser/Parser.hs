module Parser.Parser where

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Data.Char

type Parser a = StateT String Maybe a

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT
  
nxt :: Parser Char
nxt = StateT aux
  where
    aux [] = Nothing
    aux (x:xs) = Just (x,xs)

parsefail :: Parser a
parsefail = StateT $ const Nothing

sat :: (a -> Bool) -> Parser a -> Parser a
sat f prs = do
  x <- prs
  if f x then
    return x
    else
    parsefail

char :: Char -> Parser Char
char c = sat (== c) nxt

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  c <- char x
  cs <- string xs
  return (c:cs)

spaces :: Parser String
spaces = many (sat isSpace nxt)

token :: Parser a -> Parser a
token p = do
  _ <- spaces
  a <- p
  _ <- spaces
  return a

tkstr :: String -> Parser String
tkstr = token . string

