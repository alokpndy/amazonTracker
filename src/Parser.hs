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
import Control.Monad.IO.Class



--------------- START ADT 
data Item = Item
  { name :: String, unique :: Int, del :: Bool, iurl :: String, priceRecord :: [PriceDetail] 
  } deriving (Eq, Show, Generic)

type TStamp = Integer
type Price = Integer

data PriceDetail =  PriceDetail
  { dt :: UTCTime, pr ::  Integer } deriving (Eq, Show, Generic, Ord)

data ItemURL = ItemURL { url :: String } deriving (Eq, Show, Generic)

instance ToJSON ItemURL
instance FromJSON ItemURL
instance ToJSON Item
instance ToJSON PriceDetail
instance FromJSON Item
instance FromJSON PriceDetail

mkItem :: String -> Int -> Bool -> String -> UTCTime -> Maybe String -> Maybe Item
mkItem n q d u dt pr = Item n q d u <$> (mkPriceDetail dt pr) 

mkPriceDetail :: UTCTime -> Maybe String  -> Maybe [PriceDetail]
mkPriceDetail t ms = (fmap (\x -> [PriceDetail t ((read . mkDigit . parsePrice)  $ LB.pack x)] ) ms) 
--------------- END ADT



retrieveWeatherData url = do
     case parseURI url of
        Nothing  -> ioError . userError $ "Invalid URL"
        Just uri -> (NC.simpleHttp url)


retrieveItem :: String -> IO (Maybe Item) 
retrieveItem url = do  
  doc    <- retrieveWeatherData url
  price <- runX (readString [withParseHTML yes, withWarnings no] ( LB.unpack doc) >>> getAnyPrice )
  title <- runX (readString [withParseHTML yes, withWarnings no] ( LB.unpack doc) >>> getTitle)
  time <- getCurrentTime  
  return $ 
    mkItem  (stripNLandWh . mconcat$  title) (hashURL url) False url (time)  (mconcat  price) 
 
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

mkPrice s = (read . mkDigit . parsePrice) s
  

atTag tag = deep (isElem >>> hasName tag)
                        
getTitle =  deep (isElem >>> hasName "h1" >>> getChildren ) >>>
    proc p -> do
        atV <-  getAttrValue "id"  -< p
        if  atV == "productTitle" then  do
            val <- deep getText   -< p
            returnA -<    val 
        else returnA -<  atV

stripNLandWh [] = ""
stripNLandWh (x:xs)
  | x == '\n' = stripNLandWh xs
  | x == ' ' = stripNLandWh xs
  | otherwise = x : stripNLandWh xs 



getAnyPrice =  atTag "div" >>>  getChildren >>> atTag "span"  >>>
               ( hasAttrValue "id" (== "priceblock_ourprice")   `orElse`
                 hasAttrValue "id" (== "priceblock_saleprice")  `orElse`
                 hasAttrValue "id" (== "priceblock_dealprice")) >>>
               proc p -> do
                 str <- deep getText -< p
                 returnA -< Just str 
