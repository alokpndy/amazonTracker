
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

{-
--   \d schema.table  -- show table info 
-- ALTER TABLE product.trackItem DROP COLUMN itemprice;
-- select track.item.title, track.item.id, track.prices.price from track.item inner join track.prices on track.item.id = track.prices.itemid

databseURL  <- fmap (fromMaybe "No dataBase") (lookupEnv "DATABASE_URL")
conn <- connectPostgreSQL (LB.packChars databseURL)
-}

getAllItems :: Connection ->  IO (Maybe [Item])
getAllItems c = do
  xs <- liftIO $  query_ c "select *  from track.item" :: IO [(Int, Text.Text, Bool, Text.Text)]
  case xs of
    [] -> return Nothing
    ys -> do
       zs <- mapM (\(x1,x2,x3,x4) -> getYs x1) ys
       case zs of
         [] -> return Nothing
         ss -> do
           prcs <- liftIO $  mapM (\[(y1,y2)] ->   return $ PriceDetail  y1  y2) ss
           return $ Just $ (fmap (\(a,b,c,d) -> Item (Text.unpack b) a c (Text.unpack d) (prcs)) xs)
      
    
 

  where
    getYs :: Int -> IO [(Integer, Integer)]
    getYs x1 =  query c "select track.prices.timestamp, track.prices.price from track.prices where track.prices.itemid = ?"  [x1]

         


-- Saves data to disk and then return hash of the item
addItem :: Connection -> String ->  IO Item  
addItem conn s = do
  i <- retrieveItem s 
  executeMany conn  "insert into track.item (id,title, url) values (?,?,?)" [( (unique i), (name i), (iurl i)) :: (Int, String, String)]
  executeMany conn
    "insert into track.prices (id,price,timestamp,itemid) values (?,?,?)" [( (unique i), (getCost (priceRecord  i)) , (2019), (unique i) ) :: (Int, Integer, Integer, Int)]
  return  i  
 
 where
   addText :: Text.Text -> Text.Text -> Text.Text -> Text.Text
   addText x y z = Text.concat  $ x : y : z : []
   
   getCost :: [PriceDetail] -> Integer
   getCost (p:ps) =  pr p   

   parsePrice :: String -> String 
   parsePrice [] = []
   parsePrice (x:xs)
     | x == ','  =  parsePrice xs
     | x == '.' = [] 
     | otherwise = x : parsePrice xs
     
