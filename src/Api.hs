{-# LANGUAGE ViewPatterns #-}
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
import Data.Text (Text, unpack)
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
import Control.Monad.Reader
import Graphics.GChart

import Data.Time.Clock
import Database.PostgreSQL.Simple.Time

-- | Endpoints ----------------------------------------------- 
type HomeApi  = Get '[HTML] Homepage
type Homepage = H.Html

type UpdatedItemApi = "getUpdatedItem" :> ReqBody '[JSON] ItemID :> Post '[JSON] [(String , Integer)]
type ItemAllApi = "getAllItem" :> Get '[JSON] [Item]
type ItemChartApi = "getItemChart" :> Capture "id" Int :>  Get '[HTML] Homepage 

type ItemAddApi = "addItemUrl" :> ReqBody '[JSON] ItemURL :> Post '[JSON] (Maybe Item)
type UpdateExistingApi = "updateExisting" :> PutAccepted '[JSON] NoContent 
type DeleteItemApi = "deleteItem" :> Capture "id" Int  :> Delete '[JSON] NoContent  -- make DLETE request using curl 

type Api = HomeApi :<|> ItemAllApi :<|> ItemAddApi  :<|> UpdateExistingApi :<|> DeleteItemApi :<|> ItemChartApi :<|> UpdatedItemApi


-- | Server --------------------------------------------------      
server ::  Connection -> Server Api
server c = do
  homeApi  :<|> itemAllApi  :<|> itemAddApi  :<|>  itemUpdateApi  :<|> deleteApi :<|> chartApi  :<|> updatedItem
  
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
          
    chartApi :: Int -> Handler Homepage
    chartApi i = do
    
      xs <- liftIO $ getYs c i
      pxs <- return $ Prelude.map (\(x,y) -> (fromIntegral y,  makeDate2 (show x) ) ) xs
      name <- liftIO $  getOnlyTitle c i
      str <- return  $ bargraphAutoSpacing pxs $ unpack  ((!!) name 0)
      let ln = show $ (getHeight (length pxs))
      return
        $ chartHtml (H.toHtml ((!!) name 0 ))
                    (H.toValue str)
                    (H.toHtml (makeLabel ( maximum pxs)))
                    (H.toHtml (makeLabel ( minimum pxs)))
                    (H.toValue  ln)

    makeLabel :: (Int, String) -> String
    makeLabel (x1,x2) = show x1 ++ " (" ++ x2 ++ ")" 
      
    itemAddApi :: ItemURL ->  Handler (Maybe Item)
    itemAddApi i = do
      maybeyItem <- liftIO $ addItem c (url i)
      return maybeyItem

    itemUpdateApi ::  Handler NoContent 
    itemUpdateApi   = do
      liftIO $  AS.async $ updateItem c
      return NoContent 

    deleteApi :: Int -> Handler NoContent
    deleteApi i = do
      liftIO $ AS.async (delItems c i)
      return NoContent

    updatedItem :: ItemID ->  Handler [(String , Integer)]
    updatedItem i = do
      item <- liftIO $ getYs c (itmid i) 
      return $ fmap (\(x,y) -> (,) (show  x) y) item 

instance  FromHttpApiData [String] where
  parseQueryParam param = do
     s  <- parseUrlPiece param :: Either Text [String] 
     case s of
       []   -> Left $ "Unspecifed Sort Order "
       [x]  -> return  (x : [])
       xs   -> return  xs

instance  FromHttpApiData [Int] where
  parseQueryParam param = do
     s  <- parseUrlPiece param :: Either Text [Int] 
     case s of
       []   -> Left $ "No Int Values "
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
             meta ! charset "UTF-8"
             meta ! A.name "keywords" ! content " IE=edge" ! A.httpEquiv "x-ua-compatible"
             meta ! A.name "viewport" ! content "width=device-width, initial-scale=1.0"

             H.title "LoneFox: Amazon price tracker"
             H.body $ do
               H.h1 "Add Item to Track"
              
      i -> do
       H.body $ do
          h1 ! A.style "text-align: center;" $ "LoneFox: An amazon.in price tracker"  
          hr
          H.title "LoneFox: Amazon price tracker"
          h1 ! class_ "site-title" ! A.style "text-align: center;" $ mempty
          H.body $ do
             H.table $ H.style $ toHtml ("width: 516px; border-color: rgba(34, 0, 51, 0)" :: Text)
             mapM_  (\(x1,x2,x3,x4,x5,x8,x6,x7, xi)
                           --   name url addPrice addDate lowPrice lowDate cPrice cDate id textCol 
                   ->  showHtml x1
                                x2
                                x3
                                (makeDate x4)
                                x5
                                (makeDate x8)
                                x6
                                (makeDate x7)
                                xi
                                (selectTextColor (htmlToIntg x5) (htmlToIntg x6)))
                                i  
             return ()


green =  A.style "color: #39891b;"-- background-color: #d6fbdf;"
red =  A.style "color: #e65733;"-- background-color: #f5cab9;"

htmlToIntg :: Html -> Integer
htmlToIntg h = (read . renderHtml) h :: Integer 
               
selectTextColor :: Integer -> Integer -> Attribute 
selectTextColor lp cp
    | cp > lp = red
    | cp <= lp = green 

makeDate :: Html -> Html
makeDate xs  = let ys = renderHtml xs   
                   day = (takeWhile (/= ' ')) ys
                   dat = ((takeWhile (/= '.')) . (dropWhile (/= ' ')))  ys
                   in  ( H.toHtml ( day <> " \n"  <>  dat))

makeDate2 :: String -> String
makeDate2 xs  = let  
                   day = (takeWhile (/= ' ')) xs
                   dat = ((takeWhile (/= '.')) . (dropWhile (/= ' ')))  xs
                   in   day -- <> " | "  <>  dat)

setColor :: Html -> Html -> Html
setColor x y = let x1 = read $ renderHtml x :: Integer
                   y1 = read $ renderHtml y :: Integer 
                   in case (x1 > y1) of
                        True -> H.toHtml ("#39891b" :: String)
                        False -> H.toHtml ("#7f8c9f" :: String)
                           

makeTable :: Item -> (Html, AttributeValue, Html, Html, Html, Html, Html, Html, Html) --  name url addP addD lowPrice cPrice cDate id 
makeTable it =  ((H.toHtml  . (Parser.name)) it, (H.toValue . iurl) it,
                ((H.toHtml . show . getAddedtPrice . priceRecord) it),
                ((H.toHtml . getAddedtDate . priceRecord) it),
                ((H.toHtml . show . getAvgPrice . priceRecord) it),
                ((H.toHtml  . getAvgDate . priceRecord) it),
                ((H.toHtml . show .getRecentPrice . priceRecord) it),
                ((H.toHtml . getRecentDate . priceRecord) it),
                ((H.toHtml . unique) it))
             
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
                
     





showHtml  name url addP addD lowPrice lowDate cPrice cDate ids txtCol = do
  H.head $ do 
    meta ! A.name "viewport" ! content "width=device-width, initial-scale=1.0"
  body $ do
    p mempty
    table ! A.style "width: 300px;  height: 40px; margin-left: auto; margin-right: auto;"
      $ tbody $ tr ! A.style "height: 40px;"
      $ td ! A.style "width: 300px; height: 40px;"
      $ h3 ! A.style "text-align: left;"
      $ H.span ! A.style "color: #0e37f3;"
      $ H.span ! A.style "caret-color: #0e37f3"
      $ a ! href  (toValue (toValue url)) ! target (toValue (toValue url)) ! rel "noopener"
      $ name
    table ! A.style "width: 300px; height: 75px; border-color: #eceef0; margin-left: auto; margin-right: auto;"  $  tbody $ do
      tr ! A.style "height: 25.75px;" $ do
          td ! A.style "width: 150px; height: 20px; text-align: left;" $  H.span ! A.style "color: #7f8c9f;" $ addP
          td ! A.style "width: 150px; height: 20px; text-align: left;" $  H.span ! txtCol $ " " <> cPrice <> " "
      tr ! A.style "height: 18px;" $ do
          td ! A.style "width: 150px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ addD
          td ! A.style "width: 150px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;" $ cDate
      tr ! A.style "height: 18px;" $ do
          td ! A.style "width: 150px; height: 18px; text-align: left;"  $ H.span ! A.style "color: #7f8c9f;" $ "ADDED ON"
          td ! A.style "width: 140px; height: 18px; text-align: left;"  $ H.span ! A.style "color: #7f8c9f;" $ "CURRENT"
          td ! A.style "width: 10px; height: 18px; text-align: left;" $ H.span ! A.style "color: #7f8c9f;"
            $  a ! href  (toValue (baseUR  (renderHtml ids))) ! target (toValue (renderHtml ">"))  $ ">"

baseUR ::  String -> String
baseUR  id    = "https://lonefox.herokuapp.com/getItemChart/" <>  id  
--baseUR  id    = "http://localhost:3000/getItemChart/" <>  id 

chartHtml title chartLink maxP minP ht = do
  H.head $ do 
    meta ! A.name "viewport" ! content "width=device-width, initial-scale=1.0"
  body $ do 
    table ! A.style "height: 447px; margin-left: auto; margin-right: auto;" ! width "320" $ tbody $ do
      tr $ td ! A.style "width: 310px;" $ h2  ("Product: " <> title) 
      tr $ td ! A.style "width: 310px;" $ img ! src chartLink ! width "300" ! height ht
      tr $ td ! A.style "width: 310px; text-align: left;" $
        table ! A.style "height: 6px; margin-left: auto; margin-right: auto;" ! width "320" $ tbody $ tr $ do
          td ! A.style "width: 148.5px; text-align: left;" $ h4 "Lowest: " <> minP
          td ! A.style "width: 148.5px; text-align: right;" $ h4 "Highest: " <> maxP



bargraphAutoSpacing :: [(Int, String)] -> String -> String 
bargraphAutoSpacing xs name = getChartUrl $ do
                                       let priceList = Prelude.map fst xs
                                       
                                       let max = fromIntegral $ maximum priceList :: Float
                                       let labels = Prelude.map (\(x,y) -> show x ++ " (" ++ y ++ ") "  ) xs 
                                       let ys = Prelude.map (\x -> (*) 100 $ fromIntegral x / max) priceList  :: [Float]
                                       let lsCount = length xs 
                                       --setChartTitle name 
                                      
                                       setChartSize 300  (getHeight lsCount)
                                       setChartType BarHorizontalGrouped
                                       setDataEncoding Graphics.GChart.text
                                       addAxis $ makeAxis { axisType = AxisLeft
                                                          , axisLabels = Just (reverse labels) } 
                                       addChartData ys
                                       setColors ["4d89f9"]
                                       setBarWidthSpacing automatic

                            
getHeight :: Int -> Int
getHeight i
       | i <= 1 = 30 
       | i <= 2 = 60 
       | i <= 5 =  100
       | i <= 10 = 200
       | i <= 15 = 250 
       | i <= 20 = 400
       | i <= 25 = 450
       | i <= 30 = 600
       | i <= 35 = 650 
       | i <= 40 = 800
       | otherwise = 900                               
                                   





