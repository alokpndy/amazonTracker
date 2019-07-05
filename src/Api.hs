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


-- | Endpoints ----------------------------------------------- 
       
type ItemAllApi = "getAllItem" :> Get '[JSON] [Item]
--type ItemQueryApi = "vocabGetBy" :> Capture "itemId" Integer :> Get '[JSON] [Item]
--type ItemDeleteApi = "itemDelete" :> Capture "itemId" Integer :> DeleteNoContent '[JSON] NoContent

type Api = ItemAllApi -- :<|> ItemQueryApi :<|> ItemDeleteApi


-- | Server --------------------------------------------------      
server :: Server Api
server    = do
  itemAllApi -- :<|> itemQueryApi :<|> itemDeleteApi  
  
  where
    itemAllApi ::   Handler [Item]
    itemAllApi = do
      items <- liftIO retrieveItem
      return [items] 
{-       
    itemQueryApi :: Integer ->  Handler [Item]
    itemQueryApi i = do
      items <- retrieveItem
      return $ filter (\x -> itemId == i) items 
      

    itemDeleteApi :: Integer -> Handler NoContent
    itemDeleteApi i = do
      items <- retrieveItem
      filter (\x -> itemId == i) items
-}



-- | Deploy --------------------------------------------------
main2 :: Int -> IO ()
main2 port = do
  run port $ (serve (Proxy @Api) server)

             
-- http://localhost:3000/getAllItem
