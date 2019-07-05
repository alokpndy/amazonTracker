module Main where

import Api
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  
  main2 (read port)
