{-# LANGUAGE OverloadedStrings #-}

module JsonType where

import Data.Aeson


data Feature = Feature { properties :: Properties
					   , geometry :: Geometry
					   } deriving (Eq, Ord, Show)

data Properties = Properties { mag :: Double
							 , place :: !Text
							 , time :: Int
							 , url :: !Text
							 } deriving (Eq, Ord, Show)

data Geometry = Geometry { coordinates :: [Int] } deriving (Eq, Ord, Show)

data Coordinates = Coordinates { latitude :: Double
							   , longitude :: Double
							   , depth :: Double} deriving (Eq, Ord, Show)