
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
import Parser

import Data.Time.Clock


import Database.PostgreSQL.Simple.Time
import qualified  Data.ByteString as B
{-
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
-- select track.item.title, track.item.id, track.prices.price from track.item inner join track.prices on track.item.id = track.prices.itemid

databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
conn <- connectPostgreSQL (LB.packChars databseURL)
-}

getAllItems :: Connection ->  IO (Maybe [Item])
getAllItems c = do
  xs <- liftIO $  query_ c "select *  from track.items" :: IO [(Int, Text.Text, Bool, Text.Text)]
  case xs of
    [] -> return Nothing
    ys -> do
       case ys of
         [] -> return Nothing
         ls  -> do
                 prcs <- liftIO $  (traverse) (\(y1,y2,y3,y4) -> do 
                                                  ps <-  (fmap . fmap) (\(x1,x2) -> PriceDetail (eitherRight  x1)  x2) (getYs c y1)  
                                                  return $ Item (Text.unpack y2) y1 y3 (Text.unpack y4) (ps)
                                               ) ls 
                 liftIO $ return $ Just prcs
                 
                 
getYs :: Connection -> Int -> IO [(LocalTimestamp , Integer)]
getYs c x1 =  query c "select track.prices.time, track.prices.price from track.prices where track.prices.itemid = ?"  [x1] 

eitherRight :: LocalTimestamp -> UTCTime
eitherRight x =  read $ show x
 

              
-- Saves data to disk and then return hash of the item
addItem :: Connection -> String ->  IO Item  
addItem conn s = do
  i <- retrieveItem s 
  executeMany conn  "insert into track.items (id,title, url) values (?,?,?)" [( (unique i), (name i), (iurl i)) :: (Int, String, String)]
  executeMany conn
    "insert into track.prices (price,itemid) values (?,?)" [((getCost (priceRecord  i)) , (unique i) ) :: (Integer, Int)]
  return  i  
 
 where
   addText :: Text.Text -> Text.Text -> Text.Text -> Text.Text
   addText x y z = Text.concat  $ x : y : z : []
   
   getCost :: [PriceDetail] -> Integer
   getCost (p:ps) =  pr p   

   
   
updateItem :: Connection  -> IO ()
updateItem conn  = do
   x <- getAllItems conn :: IO (Maybe [Item])
   (traverse . traverse)  (updatePrice conn) x
   return () 
 
 
   
   
  

updatePrice :: Connection -> Item -> IO ()  
updatePrice conn s = do
  i <- retrieveItem ( iurl s) 
  executeMany conn
    "insert into track.prices (price,itemid) values (?,?)" [((getCost (priceRecord  i)) , (unique i) ) :: (Integer, Int)]
  return  ()  
 
 where
  
   getCost :: [PriceDetail] -> Integer
   getCost (p:ps) =  pr p   

