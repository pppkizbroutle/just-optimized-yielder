module Main where

import Parser.Parser
import Parser.Grammar
import Compiler.Analyze as A
import qualified Compiler.Translate as T

import Data.List
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Debes escribir la ruta de un archivo"
    [x] -> compile x
    _ -> putStrLn "..."

compile :: String -> IO ()
compile x = do
  content <- readFile x
  let r = parse grammar content
  case r of
    Nothing -> putStrLn "Parse fail"
    Just (y,rest) -> do
      if not $ null rest then
        putStrLn $ ("Parse fail:\n" ++ show y)
        else do
        let (xs, ys) = verify y
        putStrLn "import Data.List\nimport Test.QuickCheck\n\n"
        putStrLn $ "joyInit = " ++ show (A.init ys) ++ "\n\n"
        putStrLn $ intercalate "\n\n" $ map T.compileMain xs
