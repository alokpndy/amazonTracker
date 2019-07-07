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
import Data.Time.Clock 



-- | Endpoints ----------------------------------------------- 
       
type ItemAllApi = "getAllItem" :> Get '[JSON] (Maybe [Item])
type ItemAddApi = "addItemUrl" :> ReqBody '[JSON] ItemURL :> Post '[JSON] Item


type Api = ItemAllApi :<|> ItemAddApi  


-- | Server --------------------------------------------------      
server ::  Connection -> Server Api
server c = do
  itemAllApi  :<|> itemAddApi  
  
  where
    itemAllApi ::   Handler (Maybe [Item])
    itemAllApi = do
      item <- liftIO $ getAllItems c
      case item of
        Nothing -> return Nothing
        Just xs -> return $ Just xs
        
    itemAddApi :: ItemURL ->  Handler Item
    itemAddApi i = do
      item <- liftIO $ addItem c (url i)
      return item


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
