{-# LANGUAGE OverloadedStrings #-} 

module Persist where

import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple
import Parser
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Internal as LB


--{
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
--}
persisitData = do
  databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL (LB.packChars databseURL)
  executeMany conn  "insert into product.trackItem (id,itemName) values (?,?)" [("iddd", "Phone") :: (String, String)]

  [Only x ] <- query_ conn "select * from product.trackItem"

  putStrLn x
 
