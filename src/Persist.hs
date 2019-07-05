{-# LANGUAGE OverloadedStrings #-} 

module Persist where

import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple
import Parser
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Internal as LB
import qualified Data.Text as Text
import Data.Foldable
import Data.Monoid
import Control.Monad.IO.Class
{-
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
-}

persisitData :: IO [Text.Text]
persisitData = do
  databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
  conn <- connectPostgreSQL (LB.packChars databseURL)
--  executeMany conn  "insert into track.item (id,title, url) values (?,?,?)" [(200, "Title New", "https://") :: (Integer, String, String)]
  xs <- liftIO $  query_ conn "select id, title, url  from track.item" :: IO [(Integer, Text.Text, Text.Text)]
  mapM  (\(x1,x2,x3) ->  return  (addText  x2 x3 (Text.pack ( show (x1 :: Integer))))  )  xs
 
 where
   addText :: Text.Text -> Text.Text -> Text.Text -> Text.Text
   addText x y z = Text.concat  $ x : y : z : [] 

