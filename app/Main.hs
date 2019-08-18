module Main where

import Api
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Internal as LB



main :: IO ()
main = do
  {-
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  databseURL  <- fmap (fromMaybe "=postgresql://localhost/amazonlocaldb") (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL (LB.packChars databseURL)
  main2 conn (read port)
 -}

  
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")

--  databseURL  <- fmap (fromMaybe "DATABASE_UR=postgresql://localhost/amazonlocaldb") (lookupEnv "DATABASE_URL")
  databseURL  <- (lookupEnv "DATABASE_URL")
  case databseURL of
    Nothing -> do
               conn <- connect defaultConnectInfo { connectDatabase = "lonefoxdb" }
               main2 conn (read port)
             
    Just c ->  do
               conn <- connectPostgreSQL (LB.packChars c)
               main2 conn (read port)
           
   

