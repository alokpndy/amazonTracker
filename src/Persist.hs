{-# LANGUAGE OverloadedStrings #-} 

module Persist where

import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple
import Parser
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Internal as LB
import qualified Data.Text as Text
import Data.Foldable 
{-
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
-}

persisitData :: IO String
persisitData = do
  databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL (LB.packChars databseURL)
--  executeMany conn  "insert into track.item (id,title, url) values (?,?,?)" [(200, "Title New", "https://") :: (Integer, String, String)]
  [(x1,x2,x3)] <- query_ conn "select id, title, url from track.item"
  return $ Text.unpack x3 ++  Text.unpack x2 ++ " is " ++ show (x1 :: Int)
 

