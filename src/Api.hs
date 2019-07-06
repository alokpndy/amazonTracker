{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where


import Network.Wai.Handler.Warp (run)
import Data.Aeson 
import Servant
import GHC.Generics
import GHC.TypeLits
import Data.Text (Text)
import Data.List
import qualified Data.Text.IO   as T
import Control.Monad.IO.Class
import Servant.HTML.Blaze
import Text.Blaze.Html5 hiding (main)
import Parser 
import Persist
import Data.Traversable
import Database.PostgreSQL.Simple


itemURL = "https://www.amazon.in/dp/B07JB8DWGT/?coliid=I267DRVBQ4ERVW&colid=3KKERNQ9EEMXC&psc=1&ref_=lv_ov_lig_dp_it"


-- | Endpoints ----------------------------------------------- 
       
type ItemAllApi = "getAllItem" :> Get '[JSON] (Maybe [Item])
type ItemAddApi = "addItenUrl" :> Capture "urls" [String] :> Get '[JSON] [Item]
--type ItemDeleteApi = "itemDelete" :> Capture "itemId" Integer :> DeleteNoContent '[JSON] NoContent

type Api = ItemAllApi :<|> ItemAddApi --  :<|> ItemDeleteApi


-- | Server --------------------------------------------------      
server ::  Connection -> Server Api
server c = do
  itemAllApi  :<|> itemAddApi --  :<|> itemDeleteApi  
  
  where
    itemAllApi ::   Handler (Maybe [Item])
    itemAllApi = do
     -- items <- liftIO retrieveItem
      item <- liftIO $ getAllItems c
      case item of
        Nothing -> return Nothing
        Just xs -> return $ Just xs 
     
      
    itemAddApi :: [String] ->  Handler [Item]
    itemAddApi urls = do
      items <- liftIO $ traverse (addItem c) urls 
      return items  
{-       

    itemDeleteApi :: Integer -> Handler NoContent
    itemDeleteApi i = do
      items <- retrieveItem
      filter (\x -> itemId == i) items
-}



instance  FromHttpApiData [String] where
  parseQueryParam param = do
     s  <- parseUrlPiece param :: Either Text [String] 
     case s of
       []   -> Left $ "Unspecifed Sort Order "
       [x]  -> return  (x : [])
       xs   -> return  xs
      

-- | Deploy --------------------------------------------------
main2 :: Connection ->  Int -> IO ()
main2 c port = do
  run port $ (serve (Proxy @Api) (server c) )

             
-- http://localhost:3000/getAllItem
