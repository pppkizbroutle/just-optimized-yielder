module Main where

import Parser.Parser
import Parser.Grammar
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
    Just (y,_) -> do
      putStrLn "import Data.List\nimport Test.QuickCheck\n\n"
      putStrLn $ intercalate "\n\n" $ map T.compile y
