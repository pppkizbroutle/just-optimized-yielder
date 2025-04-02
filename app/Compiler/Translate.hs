module Compiler.Translate where

import Parser.Grammar

import Data.List

compile :: JoyAST -> String
compile (Terminal xs) = "return \"" ++ xs ++ "\"" 
compile (Identifier xs) = "chooseInt (0,joyDepth - 1) >>= (`mk_" ++ intercalate "_" (words xs) ++ "` joyWidth)"
compile (Reserved _) = undefined
compile (Repetition j) = "chooseInt (0,joyWidth) >>= (concat <$>) . (`vectorOf` " ++ compile j ++ ")"
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
compile (Rule x (Alternation js)) =
  let
    fj = filter fst js
    sj = filter (not . fst) js
    var = intercalate "_" (words x)
  in
    if not (null fj) && not (null sj) then
      "mk_" ++ var ++ " 0 joyWidth = " ++ compile (Alternation fj) ++ "\nmk_" ++ var ++ " joyDepth joyWidth = " ++ compile (Alternation sj)
    else "mk_" ++ var ++  " joyDepth joyWidth = " ++ compile (Alternation js)
compile (Rule x j) =
  let
    var = intercalate "_" (words x)
  in
    "mk_" ++ var ++  " joyDepth joyWidth = " ++ compile j
