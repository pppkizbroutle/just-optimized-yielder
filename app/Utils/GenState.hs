module Utils.GenState where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Test.QuickCheck

type VariableEnv = [(String, Int)]

type BreakStack = [(String, (Int, Int))]

data JoyState = JoyState { variables :: VariableEnv
                         , breaks :: BreakStack }

type GenState a = StateT JoyState Gen a

readVar :: String -> GenState Int
readVar xs = do
  st <- get
  let env = variables st
  case lookup xs env of
    Just i -> return i
    Nothing -> error "Variable no definida"

setBreak :: String -> GenState ()
setBreak var = do
  st <- get
  let env = variables st
  let brk = breaks st
  let filteredBreaks = filter ((== var) . fst) brk
  case filteredBreaks of
    [] -> case lookup var env of
            Just i -> modify (\s -> s{breaks = (var, (i+1,i+1)) : brk})
            Nothing -> error "Variable no definida"
    (var, (_,y)):_ -> modify (\s -> s{breaks = (var, (y+1,y+1)) : brk})

replace :: Eq x => x -> y -> [(x,y)] -> [(x,y)]
replace x y [] = []
replace x y ((z,w):zs)
  | x == z = (z,y):zs
  | otherwise = (z,w) : replace x y zs

removeFirst :: Eq x => x -> [(x,y)] -> [(x,y)]
removeFirst x [] = []
removeFirst x ((y,z):ys)
  | x == y = ys
  | otherwise = (y,z) : removeFirst x ys

clearBreak :: String -> GenState ()
clearBreak var = do
  st <- get
  let env = variables st
  let brk = breaks st
  let filteredBreaks = filter ((== var) . fst) brk
  case filteredBreaks of
    [] -> return ()
    [(x,(_,z))] -> put $ JoyState {variables = replace x z env, breaks = removeFirst x brk}
    (x,(_,y)):(_,(w,_)) : _ -> modify (\s -> s{breaks = replace x (w,y) (removeFirst x brk)})
