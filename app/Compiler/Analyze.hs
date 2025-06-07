{-# LANGUAGE TupleSections #-}
module Compiler.Analyze where

import Parser.Grammar

import Data.List

type Env = [(String, Int)]

hasEnum :: Rule -> Bool
hasEnum (Rule _ joy) = auxEnum joy
  where
    auxEnum :: JoyAST -> Bool
    auxEnum j =
      case j of
        Enum -> True
        Alternation xs -> any auxEnum (map snd xs)
        Concatenation xs -> any auxEnum xs
        Group x -> auxEnum x
        Option x -> auxEnum x
        Repetition x _ -> auxEnum x
        _ -> False

listEnum :: [Rule] -> [String]
listEnum = map getString . filter hasEnum

init :: [a] -> [(a,Int)]
init = map (, -1)

hasRepeated :: [Rule] -> Bool
hasRepeated = any (> 1) . map length . group . sort . map getString

getString :: Rule -> String
getString (Rule xs _) = xs

verify :: [Rule] -> ([Rule], [String])
verify xs = if hasRepeated xs then
              error "There are repeated definitions"
            else
              (xs, listEnum xs)
