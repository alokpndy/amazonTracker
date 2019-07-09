{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent.Async

import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import Control.Monad (forM_)
import Text.Blaze.Html5.Attributes as A

-- | Endpoints ----------------------------------------------- 
type HomeApi  = Get '[HTML] Homepage
type Homepage = H.Html
       
type ItemAllApi = "getAllItem" :> Get '[JSON] (Maybe [Item])
type ItemAddApi = "addItemUrl" :> ReqBody '[JSON] ItemURL :> Post '[JSON] Item
type UpdateExistingApi = "updateExisting" :> PutAccepted '[JSON] NoContent 
type DeleteItemApi = "deleteItem" :> Capture "id" Int  :> Delete '[JSON] NoContent  -- make DLETE request using curl 

type Api = HomeApi :<|> ItemAllApi :<|> ItemAddApi  :<|> UpdateExistingApi :<|> DeleteItemApi


-- | Server --------------------------------------------------      
server ::  Connection -> Server Api
server c = do
  homeApi :<|> itemAllApi  :<|> itemAddApi  :<|>  itemUpdateApi  :<|> deleteApi 
  
  where
    homeApi :: Handler Homepage
    homeApi =  do
      item <- liftIO $ getAllItems c
      return $ myHome item
  
    
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

    itemUpdateApi ::  Handler NoContent 
    itemUpdateApi   = do
      liftIO $  updateItem c
      return NoContent 

    deleteApi :: Int -> Handler NoContent
    deleteApi i =do
      liftIO $ delItems c i
      return NoContent
      

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


myHome :: Maybe [Item]  -> Homepage
myHome it = H.docTypeHtml $ do
    case ((fmap . fmap) makeTable it) of
      Nothing -> do
         H.head $ do
             H.title "LoneFox: Amazon price tracker"
             H.body $ do
               H.h1 "Add Item to Track"
              
      Just i -> do
          H.head $ do
             H.title ! A.style "width: 511px;" $ h1 ! A.style "text-align: center;" $ "LoneFox: amazon price tracker"
            
             H.table $ H.style $ toHtml ("width: 516px; border-color: rgba(34, 0, 51, 0)" :: Text)
             mapM_  (\(x1,x2,x3,x4,x5,x8,x6,x7) ->  showHtml x1 x2 x3 x4 x5 x8 x6 x7 ) i 
             return ()

makeTable :: Item -> (Html, AttributeValue, Html, Html, Html, Html, Html, Html) --  name url addP addD lowPrice cPrice cDate
makeTable it =  ((H.toHtml .  Parser.name) it, (H.toValue . iurl) it,
                ((H.toHtml . show . getAddedtPrice . priceRecord) it),
                ((H.toHtml . getAddedtDate . priceRecord) it),
                ((H.toHtml . show . getAvgPrice . priceRecord) it),
                ((H.toHtml  . getAvgDate . priceRecord) it),
                ((H.toHtml . show .getRecentPrice . priceRecord) it),
                ((H.toHtml . getRecentDate . priceRecord) it) )
             
getRecentPrice :: [PriceDetail] -> Integer
getRecentPrice pd = case pd of
  [] ->  0
  [x] ->  (pr x)
  xs ->   (pr . last) xs

getRecentDate :: [PriceDetail] -> String
getRecentDate pd = case pd of
  [] ->  ""
  [x] ->  show (Parser.dt x)
  xs ->  show $  (Parser.dt . last) xs

getAddedtPrice :: [PriceDetail] -> Integer
getAddedtPrice pd = case pd of
  [] ->  0
  [x] ->  (pr x)
  xs ->   pr $ Prelude.head xs 

getAddedtDate :: [PriceDetail] -> String
getAddedtDate pd = case pd of
  [] ->  "" 
  [x] ->  show (Parser.dt x)
  xs ->  show $  (Parser.dt . Prelude.head) xs

getAvgDate :: [PriceDetail] -> String
getAvgDate it = case it of 
    [] ->  ""
    [x] ->  (show . Parser.dt $ x)
    xs ->   let minP =  foldr1 Prelude.min $ fmap pr  xs in 
                  (show . Parser.dt . Prelude.head) (filter (\x -> Parser.pr x == minP) xs)
getAvgPrice :: [PriceDetail] -> Integer
getAvgPrice it = case it of 
    [] ->  0
    [x] ->  (pr x)
    xs ->   foldr1 Prelude.min $ fmap pr  xs 
                
     

showHtml  name url addP addD lowPrice lowDate cPrice cDate = do 
  p mempty
  table ! A.style "height: 98px; width: 90%; margin-left: auto; margin-right: auto;" $ tbody $ do
    tr $ td ! A.style "width: 511px;" $ do
        h1 mempty
        table ! A.style "height: 99px; margin-left: auto; margin-right: auto;" ! width "500" $ tbody $ tr $ td ! A.style "width: 440px;" $ do
            h2 ! A.style "text-align: left;" $ do
                strong $ a ! href "Link" ! target url ! rel "noopener" $ "link  "
                name
           -- h2 $ H.span ! A.style "color: #000000;" $ strong $ a ! A.style "color: #000000;" ! href url ! target "_blank" ! rel "noopener" $ name

            table ! A.style "height: 56px; margin-left: auto; margin-right: auto;" ! width "100%" $ tbody $ tr $ do
              
                td ! A.style "width: 138.34375px; text-align: left;" $ do
                    h3 ! class_ "line-height:0.1;" $ H.span ! A.style "color: #808080;" $ addP
                    p $ H.span ! A.style "color: #808080;" $ addD
                td ! A.style "width: 138.34375px;" $ do
                    h3 ! class_ "line-height:0.1; text-align: left;" $ H.span ! A.style "color: #808080;" $ lowPrice
                    p $ H.span ! A.style "color: #808080;" $ lowDate
                td ! A.style "width: 138.34375px; text-align: left;" $ do
                    h3 ! class_ "line-height:0.1;" $ H.span ! A.style "color: #008000;" $ cPrice
                    p cDate

