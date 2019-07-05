{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser where

import           Control.Arrow
import           Control.Category
import           Network.URI
import           Prelude           hiding (id, (.))
import           Text.XML.HXT.Core
import Data.Monoid
import qualified Network.HTTP.Conduit as NC

import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics
import Data.Aeson 

itemURL = "https://www.amazon.in/dp/B07JB8DWGT/?coliid=I267DRVBQ4ERVW&colid=3KKERNQ9EEMXC&psc=1&ref_=lv_ov_lig_dp_it"
retrieveWeatherData = do
     case parseURI itemURL of
        Nothing  -> ioError . userError $ "Invalid URL"
        Just uri -> (NC.simpleHttp itemURL)


data Item = Item { pPrice  :: String, pTitle :: String, itemID :: Integer, idDeleted :: Bool } deriving (Eq, Show, Generic) 

instance ToJSON Item

retrieveItem = do
  doc    <- retrieveWeatherData
  price <- runX (getwhole $ LB.unpack doc)
  title <- runX (readString [withParseHTML yes, withWarnings no] ( LB.unpack doc) >>> getTitle)
  return $  Item (parsePrice  $ LB.pack ( mconcat  price)) (stripNLandWh . mconcat$  title) 0 False
  
parsePrice :: LB.ByteString -> String
parsePrice bs = (LB.unpack . last . LB.words) bs 
 
atTag tag = deep (isElem >>> hasName tag)

getwhole file = readString [withParseHTML yes, withWarnings no] file >>>
                deep (isElem >>> hasName "body" >>> getChildren) >>>
                proc x -> do
                productPrice <- listA getPrice -< x
                returnA -<  (filterPrice $ filter (not . null) ( productPrice ))
                where
                  filterPrice xs = case xs of
                    [] -> ""
                    [x] -> x
                    (x:xs) -> x 
                        
getTitle =  deep (isElem >>> hasName "h1" >>> getChildren ) >>>
    proc p -> do
        atV <-  getAttrValue "id"  -< p
        if  atV == "productTitle" then  do
            val <- deep getText   -< p
            returnA -<    val 
        else returnA -<  atV

getPrice = atTag "div" >>> getChildren >>> atTag "span" >>> getChildren >>>
    proc p -> do
        atV <-  getAttrValue "class"  -< p
        if  atV == "a-color-price" then  do
            val <- deep getText   -< p
            returnA -<    val 
        else returnA -<  ""


stripNLandWh [] = ""
stripNLandWh (x:xs)
  | x == '\n' = stripNLandWh xs
  | x == ' ' = stripNLandWh xs
  | otherwise = x : stripNLandWh xs 
