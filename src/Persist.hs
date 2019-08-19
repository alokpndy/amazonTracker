
{-# LANGUAGE OverloadedStrings #-} 

module Persist where

import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple 
import Parser
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Internal as LB
import  Data.Text (Text, pack, unpack, concat) 
import Data.Foldable
import Data.Monoid
import Control.Monad.IO.Class
import Parser

import Data.Time.Clock
import Data.List

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import qualified  Data.ByteString as B



{-
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
-- select track.item.title, track.item.id, track.prices.price from track.item inner join track.prices on track.item.id = track.prices.itemid

databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
conn <- connectPostgreSQL (LB.packChars databseURL)
-}
instance FromRow Text where
  fromRow = field 

getOnlyTitle :: Connection -> Int -> IO [Text]
getOnlyTitle c x1 = query c "select track.items.title FROM track.items WHERE track.items.id = ?"  [x1] :: IO  [Text]



getAllItems :: Connection ->  IO [Item]
getAllItems c = do
  xs <- liftIO $  query_ c "select *  from track.items" :: IO [(Int, Text, Bool, Text, Integer)]
  case xs of
    [] -> return []
    ys -> do
       case ys of
         [] -> return []
         ls  -> do
                 prcs <- liftIO $  (traverse) (\(y1,y2,y3,y4,y5) -> do 
                                                  ps <-  (fmap . fmap) (\(x1,x2) -> PriceDetail (eitherRight  x1)  x2) (getYs c y1)  
                                                  return $ Item (unpack y2) y1 y3 (unpack y4) (ps) y5
                                               ) ls 
                 liftIO $ return  prcs
                 
                 
getYs :: Connection -> Int -> IO [(LocalTimestamp , Integer)]
getYs c x1 =  query c "select track.prices.time, track.prices.price from track.prices where track.prices.itemid = ?"  [x1]



eitherRight :: LocalTimestamp -> UTCTime
eitherRight x =  read $ show x
 

              
-- Saves data to disk and then return hash of the item
addItem ::  Connection -> String ->  IO (Maybe Item)  
addItem  conn s = do 
  getI <- retrieveItem s
  case getI of
    Nothing -> return Nothing
    Just i -> do 
      executeMany conn
        "insert into track.items (id,title, url, currentPrice) values (?,?,?,?)"
           [( (unique i), (name i), (iurl i), (cp i)) :: (Int, String, String, Integer)]
      executeMany conn "insert into track.prices (price,itemid) values (?,?)" [((getCost (priceRecord  i)) , (unique i) ) :: (Integer, Int)]
      return  $  getI  
 
 where

   addText :: Text -> Text -> Text -> Text
   addText x y z = Data.Text.concat  $ x : y : z : []
   
   getCost :: [PriceDetail] -> Integer
   getCost (p:ps) =  pr p   

delItems :: Connection -> Int -> IO ()
delItems  c x1 =  do
  liftIO $ execute c "delete from track.items where track.items.id = ?" (Only x1) 
  return () 
   
updateItem :: Connection  -> IO ()
updateItem  conn  = do
   xs <- getAllItems conn :: IO [Item]
   traverse  (updatePrice conn) xs
   return () 

updatePrice :: Connection -> Item  -> IO ()  
updatePrice conn s = do 
  oldURL <-  return $ iurl s  :: IO String
  j <- retrieveItem  oldURL :: IO(Maybe Item)  -- new data 
  case j of
    Nothing ->  return ()
    Just i -> do
          oldPrices <- query conn "select track.prices.price FROM track.prices WHERE track.prices.itemid = ?"  [(unique i)  :: Int] :: IO [Int]
          let price = (!!) (fmap pr ((priceRecord i) :: [PriceDetail]) ) 0  ::  Integer
          case (elem (fromInteger price) oldPrices) of
            True -> do
                  executeMany conn
                    "UPDATE track.items SET currentPrice = upd.x FROM (VALUES (?,?)) as upd(x,y) WHERE track.items.id = upd.y"
                        [(cp i, unique i)  :: (Integer, Int)] 
                  return ()
            False -> do
                  executeMany conn
                    "UPDATE track.items SET currentPrice = upd.x FROM (VALUES (?,?)) as upd(x,y) WHERE track.items.id = upd.y"
                        [(cp i, unique i)  :: (Integer, Int)]
              
                  executeMany
                     conn  "insert into track.prices (price,itemid) values (?,?)" [((getCost (priceRecord  i)) , (unique i) ) :: (Integer, Int)]  
                  return  ()  
 
 where
   getCost :: [PriceDetail] -> Integer
   getCost (p:ps) =  pr p   


instance FromRow Int where
  fromRow = field 






updateItem2 :: Connection  -> IO ()
updateItem2  conn  = do
   xs <- getAllItems conn :: IO [Item]
   traverse  (updateTitle conn) xs
   return () 

updateTitle :: Connection -> Item  -> IO ()  
updateTitle conn s = do 
  oldURL <-  return $ iurl s  :: IO String
  oldID <-  return $ unique s  :: IO Int
  j <- retrieveItem  oldURL :: IO(Maybe Item)  -- new data 
  case j of
    Nothing ->  return ()
    Just i -> do
          executeMany conn  "UPDATE track.items SET title = upd.x FROM (VALUES (?,?)) as upd(x,y) WHERE track.items.id = upd.y" [(name i, oldID)  :: (String, Int)]

          return  ()  

