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
import qualified Control.Concurrent.Async as AS

import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5   as H
import Control.Monad (forM_)
import Text.Blaze.Html5.Attributes as A

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
          H.title "LoneFox: Amazon price tracker"
          h1 ! class_ "site-title" ! A.style "text-align: center;" $ mempty
          H.body $ do
             showHeaderHTML
             H.table $ H.style $ toHtml ("width: 516px; border-color: rgba(34, 0, 51, 0)" :: Text)
             mapM_  (\(x1,x2,x3,x4,x5,x8,x6,x7) ->  showHtml x1 x2 x3 x4 x5 x8 x6 x7 ) i 
             return ()

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
  table ! A.style "margin-left: auto; margin-right: auto;" ! width "70%" $ tbody $ do
    tr $ do
        td ! A.style "width: 40%; text-align: left;" $ h4 $ do
            strong $ a ! href url ! target url ! rel "noopener" $ "<>"
            name
        td ! A.style "width: 20%; text-align: right;" $ do
            h4 ! class_ "line-height:0.1;" $ H.span ! A.style "color: #808080;" $ addP
            H.span ! A.style "color: #808080;" $ addD
        td ! A.style "width: 20%; text-align: right;" $ do
            h4 ! class_ "line-height:0.1;" $ H.span ! A.style "color: #808080;" $ lowPrice
            H.span ! A.style "color: #808080;" $ lowDate
        td ! A.style "width: 20%; text-align: right;" $ do
            h4 ! class_ "line-height:0.1;" $ H.span ! A.style "color: #1c6bf4;" $ cPrice
            H.span ! A.style "color: #1c6bf4;" $ cDate
  p mempty

showHeaderHTML = do
 
  table ! A.style "margin-left: auto; margin-right: auto;" ! width "70%" $ tbody $ do
    tr $ do
        td ! A.style "text-align: left; width: 40%;" $ p $ H.span ! A.style "color: #333333;" $ strong "Name of Item"
        td ! A.style "width: 20%; text-align: right;" $ p ! class_ "line-height:0.1;" $ H.span ! A.style "color: #333333;" $ strong "Added On"
        td ! A.style "width:20%; text-align: right;" $ p ! class_ "line-height:0.1;" $ H.span ! A.style "color: #333333;" $ strong "Lowest"
        td ! A.style "width: 20%; text-align: right;" $ p ! class_ "line-height:0.1;" $ H.span ! A.style "color: #333333;" $ strong "Current"
   
  hr
  p mempty
 
