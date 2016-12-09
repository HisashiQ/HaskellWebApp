module Types where

import Parser
import Data.Maybe 
import Text.Read

data Earthquake = Earthquake { year :: Maybe Int
                             , month :: Maybe Int
                             , day :: Maybe Int
                             , place :: String
                             , magnitude :: Maybe Double
                             , longitude :: Maybe Double
                             , latitude :: Maybe Double
                             , depth :: Maybe Double
                             , url :: String
                             } deriving Show 

makeEarthquake :: String -> Earthquake
makeEarthquake x = Earthquake { year = getYear $ (readMaybe $ take 10 $ getProperty "time" x)
                              , month = getMonth $ (readMaybe $ take 10 $ getProperty "time" x)
                              , day = getDay $ (readMaybe $ take 10 $ getProperty "time" x)
                              , place = init.tail $ getProperty "place" x
                              , magnitude = readMaybe $ getProperty "mag" x
                              , longitude = (getCoordinates x) !! 0
                              , latitude = (getCoordinates x) !! 1
                              , depth = (getCoordinates x) !! 2
                              , url = init.tail $ getProperty "url" x
                              }

validateEarthquake :: Earthquake -> Bool
validateEarthquake x
    | elem Nothing [year x, month x, day x] || elem Nothing [magnitude x, longitude x, latitude x, depth x] == True = False
    | otherwise = True


data Region = Region { name :: String
                     , latFrom :: Double
                     , latTo :: Double
                     , longFrom :: Double
                     , longTo :: Double
                     } deriving Show

makeRegion :: [String] -> Region
makeRegion xs = Region { name = ( xs !! 0)
                       , latFrom = ( read $ xs !! 1 )
                       , latTo = ( read $ xs !! 2)
                       , longFrom = ( read $ xs !! 3)
                       , longTo = ( read $ xs !! 4)
                       }
