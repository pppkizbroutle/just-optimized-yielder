module Compiler.Translate where

import Parser.Grammar
import Parser.Parser

import Control.Applicative

import Data.List
import Data.Char

compile :: JoyAST -> String
compile Enum = "return \"@\""
compile (Terminal xs) = "return \"" ++ xs ++ "\"" 
compile (Identifier b xs _) = if b then
                              "chooseInt (0,joyDepth - 1) >>= (`mk_" ++ intercalate "_" (words xs) ++ "` joyWidth)"
                            else "chooseInt (0,joyDepth) >>= (`mk_" ++ intercalate "_" (words xs) ++ "` joyWidth)"
compile (Reserved xs) =
  let
    result = parse reservedParser xs
  in
    case result of
      Nothing -> error "Parse error on reserved clause"
      Just (s1,s2) -> if not $ null s2 then
                        error "Parse error on reserved clause"
                      else
                        s1
compile (Repetition j Nothing) = "chooseInt (0,joyWidth) >>= (concat <$>) . (`vectorOf` (" ++ compile j ++ "))"
compile (Repetition j (Just (x,y)))
  | y <= (-1) = "chooseInt (" ++ show x ++ ",joyWidth) >>= (concat <$>) . (`vectorOf` (" ++ compile j ++ "))"
  | otherwise = "chooseInt (" ++ show x ++ "," ++ show y ++ ") >>= (concat <$>) . (`vectorOf` (" ++ compile j ++ "))"
compile (Option j) = "oneof [return \"\", " ++ compile j ++ "]"
compile (Group j) = compile j
compile (Concatenation js) =
  let
    strjs = map compile js
    listj = "[" ++ intercalate ", " strjs ++ "]"
  in
    "concat <$> sequence " ++ listj
compile (Alternation js) =
  let
    sj = map snd js
    strjs = map compile sj
    listj = "[" ++ intercalate ", " strjs ++ "]"
  in
    "oneof " ++ listj

compileMain :: Rule -> String
compileMain (Rule x (Alternation js)) =
  let
    fj = filter fst js
    sj = filter (not . fst) js
    var = intercalate "_" (words x)
  in
    if not (null fj) && not (null sj) then
      "mk_" ++ var ++ " joyDepth joyWidth | joyDepth <= 0 = " ++ compile (Alternation fj) ++ "\nmk_" ++ var ++ " joyDepth joyWidth joyNum = " ++ compile (Alternation sj)
    else "mk_" ++ var ++  " joyDepth joyWidth joyNum = " ++ compile (Alternation js)
compileMain (Rule x j) =
  let
    var = intercalate "_" (words x)
  in
    "mk_" ++ var ++  " joyDepth joyWidth joyNum = " ++ compile j

reservedParser :: Parser String
reservedParser = int32
                 <|> int64
                 <|> uint32
                 <|> uint64
                 <|> intr
                 <|> ufloat
                 <|> alphaLower
                 <|> alphaUpper
                 <|> alpha
                 <|> digit
  where
    alpha = tkstr "alphabetic" >> return "oneof [(: []) <$> chooseEnum ('a', 'z'),(: []) <$> chooseEnum ('A', 'Z')]"
    alphaLower = tkstr "alphabetic" >> tkstr "lower" >> return "(: []) <$> chooseEnum ('a', 'z')"
    alphaUpper = tkstr "alphabetic" >> tkstr "upper" >> return "(: []) <$> chooseEnum ('A', 'Z')"
    digit = tkstr "digit" >> return "show <$> chooseInt (0,9)"
    ufloat = tkstr "ufloat" >> return "(show . abs) <$> (arbitrary :: Gen Float)"
    int32 = tkstr "int" >> tkstr "32" >> return "show <$> chooseInteger (-2^31,2^31-1)"
    int64 = tkstr "int" >> tkstr "64" >> return "show <$> chooseInteger (-2^63,2^63-1)"
    uint32 = tkstr "uint" >> tkstr "32" >> return "show <$> chooseInteger (0,2^32-1)"
    uint64 = tkstr "uint" >> tkstr "64" >> return "show <$> chooseInteger (0,2^64-1)"
    intr = do
      _ <- tkstr "int" >> tkstr "between"
      s1 <- (string "-" <|> string "")
      n1 <- some $ sat isDigit nxt
      _ <- tkstr "and"
      s2 <- (string "-" <|> string "")
      n2 <- some $ sat isDigit nxt
      return $ "show <$> chooseInteger (" ++ s1 ++ n1 ++ "," ++ s2 ++ n2 ++ ")"
      
      
