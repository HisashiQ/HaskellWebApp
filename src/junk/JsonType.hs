{-# LANGUAGE OverloadedStrings #-}

module JsonType where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

data Earthquake =
  Earthquake { typeof :: Text
             , metadata :: Object
             , features :: Array
             , bbox :: Array
             } deriving (Show)

instance FromJSON Earthquake where
 parseJSON (Object v) =
    Earthquake <$> v .: "type"
           <*> v .: "metadata"
           <*> v .: "features"
           <*> v .: "bbox"
 parseJSON _ = mzero

data Features =
   Features { typefeature :: Text
            , properties :: Object
            , geometry :: Object
            , id :: String
             } deriving(Show)

instance FromJSON Features where
 parseJSON (Object v) = Features <$>
           ((v .: "features") >>= (.: "type"))
           <*> ((v .: "features") >>= (.: "properties"))
           <*> ((v .: "features") >>= (.: "geometry"))
           <*> ((v .: "features") >>= (.: "id"))

-- data Metadata =
--   Metadata { generated :: Integer
--            , url :: String
--            , title :: String
--            , status :: Int
--            , api :: String
--            , count :: Int
--            } deriving (Show)
--
-- instance FromJSON Metadata where
-- parseJSON (Object v) = Metadata <$>
--           ((v .: "metadata") >>= (.: "generated"))
--           <*> ((v .: "metadata") >>= (.: "url"))
--           <*> ((v .: "metadata") >>= (.: "title"))
--           <*> ((v .: "metadata") >>= (.: "status"))
--           <*> ((v .: "metadata") >>= (.: "api"))
--           <*> ((v .: "metadata") >>= (.: "count"))
-- parseJSON _ = mzero

-- data Person =
--   Person { firstName  :: !Text
--          , lastName   :: !Text
--          , age        :: Int
--          , likesPizza :: Bool
--            } deriving (Show,Generic)

-- data Feature = Feature {
--                  place :: !Text
--  	  					 , time :: Int
--    						 , url :: !Text
-- 					   } deriving (Eq, Ord, Show, Generic)

-- data Properties = Properties { mag :: Double
-- 							 , place :: !Text
-- 							 , time :: Int
-- 							 , url :: !Text
-- 							 } deriving (Eq, Ord, Show, Generic)
--
-- data Geometry = Geometry { coordinates :: [Int] } deriving (Eq, Ord, Show, Generic)
--
-- data Coordinates = Coordinates { latitude :: Double
-- 							   , longitude :: Double
-- 							   , depth :: Double} deriving (Eq, Ord, Show, Generic)
