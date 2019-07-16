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

import Text.Blaze.Html.Renderer.String
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
import qualified Control.Concurrent.Async as AS

import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import Control.Monad (forM_)
import Text.Blaze.Html5.Attributes as A
import Control.Monad.Except


-- | Endpoints ----------------------------------------------- 
type HomeApi  = Get '[HTML] Homepage
type Homepage = H.Html
       
type ItemAllApi = "getAllItem" :> Get '[JSON] [Item]
type ItemAddApi = "addItemUrl" :> ReqBody '[JSON] ItemURL :> Post '[JSON] (Maybe Item)
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
  
    
    itemAllApi ::   Handler  [Item]
    itemAllApi = do
      item <- liftIO $ getAllItems c
      case item of
        [] -> return []
        xs -> return  xs
        
    itemAddApi :: ItemURL ->  Handler (Maybe Item)
    itemAddApi i = do
      maybeyItem <- liftIO $ addItem c (url i)
      return maybeyItem

    itemUpdateApi ::  Handler NoContent 
    itemUpdateApi   = do
      liftIO $ AS.async $ updateItem c
      return NoContent 

    deleteApi :: Int -> Handler NoContent
    deleteApi i = do
      liftIO $ AS.async (delItems c i)
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


myHome :: [Item]  -> Homepage
myHome it = H.docTypeHtml $ do
    case (fmap makeTable it) of
      [] -> do
         H.head $ do
             H.title "LoneFox: Amazon price tracker"
             H.body $ do
               H.h1 "Add Item to Track"
              
      i -> do
       H.head $ do
          h1 ! A.style "text-align: center;" $ "LoneFox: An amazon.in price tracker"  
          hr
          H.title "LoneFox: Amazon price tracker"
          h1 ! class_ "site-title" ! A.style "text-align: center;" $ mempty
          H.body $ do
             H.table $ H.style $ toHtml ("width: 516px; border-color: rgba(34, 0, 51, 0)" :: Text)
             mapM_  (\(x1,x2,x3,x4,x5,x8,x6,x7) ->  showHtml x1 x2 x3 (makeDate x4) x5 (makeDate x8) x6 (makeDate x7) ) i    --   name url addP addD lowPrice lowDate cPrice cDate
             return ()

makeDate :: Html -> Html
makeDate xs  = let ys = renderHtml xs   
                   day = (takeWhile (/= ':')) ys
                   dat = ((takeWhile (/= '.')) . (dropWhile (/= ' ')))  ys
                   in  ( H.toHtml ( day <> " | "  <>  dat))
  

makeTable :: Item -> (Html, AttributeValue, Html, Html, Html, Html, Html, Html) --  name url addP addD lowPrice cPrice cDate
makeTable it =  ((H.toHtml .  take 40 . (\x -> x ++ ".....................................") . (Parser.name)) it, (H.toValue . iurl) it,
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
  table ! A.style "height: 30px; margin-left: auto; margin-right: auto;" ! width "595" $ tbody $ tr ! A.style "height: 18.75px;" $ td ! A.style "width: 585px; height: 18.75px;" $ h3 ! A.style "text-align: left;" $ H.span ! A.style "color: #0e37f3;" $ H.span ! A.style "caret-color: #0e37f3" $ a ! href url ! target url ! rel "noopener" $  name
  table ! A.style "height: 75px; border-color: #eceef0; margin-left: auto; margin-right: auto;"  ! width "600" $ tbody $ do
    tr ! A.style "height: 25.75px;" $ do
        td ! A.style "width: 143px; height: 25px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ addP
        td ! A.style "width: 143px; height: 25px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ lowPrice
        td ! A.style "width: 143px; height: 25px; text-align: left;" $ H.span ! A.style "color: #39891b; background-color: #d6fbdf;" $ " " <> cPrice <> " "
    tr ! A.style "height: 18px;" $ do
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ addD
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ lowDate
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ cDate
    tr ! A.style "height: 18px;" $ do
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f; background-color: #eceef0;" $ "ADDED"
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f; background-color: #eceef0;" $ "LOWEST"
        td ! A.style "width: 143px; height: 18px; text-align: left;" $ H.span ! A.style "color: #39891b; background-color: #d6fbdf;" $ "CURRENT"
