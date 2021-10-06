module Main where

import qualified CSE230.Shapes    as Shapes
import qualified CSE230.Directory as Directory 
import System.Environment            (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [] -> Shapes.main 
    _  -> Directory.main args