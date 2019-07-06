module Main where

import Api
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Internal as LB

main :: IO ()
main = do
  
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL (LB.packChars databseURL)
  main2 conn (read port)
