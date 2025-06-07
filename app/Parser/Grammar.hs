module Parser.Grammar where

import Control.Applicative
import Data.Char
import Data.List

import Parser.Parser

data Rule = Rule String JoyAST deriving Show

data JoyAST = Enum
            | Alternation [(Bool, JoyAST)]
            | Concatenation [JoyAST]
            | Group JoyAST
            | Option JoyAST
            | Repetition JoyAST (Maybe (Int, Int))
            | Reserved String
            | Identifier Bool String Int
            | Terminal String deriving Show

commentary :: Parser String
commentary = do
  _ <- string "(*"
  inside
  where
    inside = do
      c1 <- nxt
      if c1 /= '*' then do
        cs <- inside
        return $ c1:cs
        else do
        c2 <- nxt
        if c2 /= ')' then do
          cs <- inside
          return $ c1 : c2 : cs
          else
          return []

tokenJoy :: Parser a -> Parser a
tokenJoy p = do
  _ <- token (commentary <|> string "")
  x <- p
  _ <- token (commentary <|> string "")
  return x

tkJoyStr :: String -> Parser String
tkJoyStr = tokenJoy . string
  
initialWord :: Parser String
initialWord = do
  c <- sat isAlpha nxt
  cs <- many (sat (\x -> isAlpha x || isDigit x || (x == '_')) nxt)
  return (c:cs)

word :: Parser String
word = some $ sat (\x -> isAlpha x || isDigit x || (x == '_')) nxt

identifier :: Parser JoyAST
identifier = do
  sym <- tkJoyStr "*" <|> string ""
  first <- tokenJoy initialWord
  rest <- many $ tokenJoy word
  s <- strategy
  return $ Identifier (null sym) (intercalate " " $ first : rest) s
  where
    strategy :: Parser Int
    strategy = do
      s <- tkJoyStr "+" <|> tkJoyStr "*" <|> tkJoyStr "^" <|> return ""
      case s of
        [] -> return 0
        ['+'] -> return 1
        ['*'] -> return 2
        ['^'] -> return 3
        _ -> undefined

reserved :: Parser JoyAST
reserved = do
  _ <- tkJoyStr "?"
  first <- tokenJoy initialWord
  rest <- many $ tokenJoy word
  _ <- tkJoyStr "?"
  return $ Reserved $ intercalate " " $ first : rest

terminal :: Parser JoyAST
terminal = tokenJoy (single <|> double)
  where
    single = do
      _ <- string "\'"
      inside <- many (sat (/= '\'') $ nxt)
      _ <- string "\'"
      return $ Terminal inside
    double = do
      _ <- string "\""
      inside <- many (sat (/= '\"') $ nxt)
      _ <- string "\""
      return $ Terminal inside

term :: Parser JoyAST
term = terminal
       <|> identifier
       <|> reserved
       <|> grouping
       <|> option
       <|> repetition
       <|> enum

grouping :: Parser JoyAST
grouping = do
  _ <- tkJoyStr "("
  alt <- alternation
  _ <- tkJoyStr ")"
  return $ Group alt

option :: Parser JoyAST
option = do
  _ <- tkJoyStr "["
  alt <- alternation
  _ <- tkJoyStr "]"
  return $ Option alt

repetition :: Parser JoyAST
repetition = do
  _ <- tkJoyStr "{"
  alt <- alternation
  _ <- tkJoyStr "}"
  limit <- tokenJoy ((Just <$> limitAux) <|> return Nothing)
  return $ Repetition alt limit

number :: Parser Int
number = do
  xs <- some $ (sat isDigit nxt)
  return $ read xs
limitAux :: Parser (Int, Int)
limitAux = do
  _ <- tkJoyStr "{"
  n1 <- tokenJoy number
  _ <- tkJoyStr ","
  n2 <- tokenJoy number <|> (tkJoyStr "*" >> return (-1))
  _ <- tkJoyStr "}"
  return (n1,n2)

alternation :: Parser JoyAST
alternation = do
  x <- aux
  case x of
    Left (_,t2) -> return t2
    Right xs -> return $ Alternation xs
  where
    aux :: Parser (Either (Bool, JoyAST) [(Bool, JoyAST)])
    aux = multiple <|> single
    single = do
      b <- tkJoyStr "#" <|> string ""
      j <- concatenation
      return $ Left (b == "#", j)
    multiple = do
      b <- tkJoyStr "#" <|> string ""
      t <- concatenation
      _ <- tkJoyStr "|"
      rest <- aux
      case rest of
        Right ts -> return $ Right $ (b == "#", t) : ts
        Left s -> return $ Right [(b == "#", t),s]

concatenation :: Parser JoyAST
concatenation = multiple <|> single
  where
    single = term
    multiple = do
      t <- term
      _ <- tkJoyStr ","
      rest <- concatenation
      case rest of
        Concatenation ts -> return $ Concatenation (t:ts)
        _ -> return $ Concatenation [t,rest]

refs :: Parser String
refs = do
  first <- tokenJoy initialWord
  rest <- many $ tokenJoy word
  return $ intercalate " " $ first : rest

enum :: Parser JoyAST
enum = tkJoyStr "@" >> return Enum

rule :: Parser Rule
rule = do
  xs <- refs
  _ <- tkJoyStr "="
  alt <- alternation
  _ <- tkJoyStr ";"
  return $ Rule xs alt

grammar :: Parser [Rule]
grammar = many rule
