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
import Data.Hashable
import Data.Time.Clock 

{-

UTCTime	 
utctDay :: Day	
the day
utctDayTime :: DiffTime	
the time from midnight, 0 <= t < 86401s (because of leap-seconds)

-}


--------------- START ADT 
data Item = Item
  { name :: String, unique :: Int, del :: Bool, iurl :: String, priceRecord :: [PriceDetail] 
  } deriving (Eq, Show, Generic)

type TStamp = Integer
type Price = Integer

data PriceDetail =  PriceDetail
  { dt :: UTCTime, pr ::  Integer } deriving (Eq, Show, Generic)

data ItemURL = ItemURL { url :: String } deriving (Eq, Show, Generic)

instance ToJSON ItemURL
instance FromJSON ItemURL
instance ToJSON Item
instance ToJSON PriceDetail
instance FromJSON Item
instance FromJSON PriceDetail

mkItem :: String -> Int -> Bool -> String -> UTCTime -> Integer -> Item
mkItem n q d u dt pr = Item n q d u (mkPriceDetail dt pr) 

mkPriceDetail :: UTCTime -> Integer -> [PriceDetail]
mkPriceDetail d p = PriceDetail d p : []
--------------- END ADT



retrieveWeatherData url = do
     case parseURI url of
        Nothing  -> ioError . userError $ "Invalid URL"
        Just uri -> (NC.simpleHttp url)


retrieveItem :: String -> IO Item 
retrieveItem url = do  
  doc    <- retrieveWeatherData url
  price <- runX (getwhole $ LB.unpack doc)
  title <- runX (readString [withParseHTML yes, withWarnings no] ( LB.unpack doc) >>> getTitle)
  time <- getCurrentTime
  return $ 
    mkItem  (stripNLandWh . mconcat$  title) (hashURL url) False url (time)  ((read . mkDigit . parsePrice)  $ LB.pack ( mconcat  price))
 
mkDigit :: String -> String 
mkDigit [] = []
mkDigit (x:xs)
     | x == ','  =  mkDigit xs
     | x == '.' = [] 
     | otherwise = x : mkDigit xs

hashURL :: String -> Int
hashURL s = hash  s 

parsePrice :: LB.ByteString -> String
parsePrice bs = (LB.unpack . last . LB.words) bs 
 
atTag tag = deep (isElem >>> hasName tag)

getwhole file = readString [withParseHTML yes, withWarnings no] file >>>
                deep (isElem >>> hasName "body" >>> getChildren) >>>
                proc x -> do
                    dPrice <- listA getPriceD -< x
                    nPrice <- listA getPriceN  -< x
                    returnA -<  (filterPrice $ filter (not . null) ( if (length dPrice > 1) then dPrice else nPrice )) 
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

filterTag tag = processTopDown (filterA $  (hasName tag))

stripNLandWh [] = ""
stripNLandWh (x:xs)
  | x == '\n' = stripNLandWh xs
  | x == ' ' = stripNLandWh xs
  | otherwise = x : stripNLandWh xs 



getPriceN = atTag "div" >>>  getChildren >>> atTag "span" >>> 
    proc p -> do
        atV <-  getAttrValue "id"  -< p
        if  atV == "priceblock_ourprice" then  do
            val <- deep getText   -< p
            returnA -<    val 
        else returnA -<  ""

getPriceD = atTag "div" >>>  getChildren >>> atTag "span" >>> 
    proc p -> do
        atV <-  getAttrValue "id"  -< p
        if  atV == "priceblock_dealprice" then  do
            val <- deep getText   -< p
            returnA -<    val 
        else returnA -<  ""
