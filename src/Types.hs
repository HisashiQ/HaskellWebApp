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

getEarthquakeData :: String -> Earthquake
getEarthquakeData rawString = makeEarthquake rawString rawTime coordinates
    where
      rawTime = take 10 $ getProperty "time" rawString
      coordinates = getCoordinates rawString

makeEarthquake :: String -> String -> [Maybe Double] -> Earthquake
makeEarthquake rawString rawTime coordinates = Earthquake { year = getYear $ readMaybe $ rawTime
                                                             , month = getMonth $ readMaybe $ rawTime
                                                             , day = getDay $ readMaybe $ rawTime
                                                             , place = init.tail $ getProperty "place" rawString
                                                             , magnitude = readMaybe $ getProperty "mag" rawString
                                                             , longitude = coordinates !! 0
                                                             , latitude = coordinates !! 1
                                                             , depth = coordinates !! 2
                                                             , url = "-"
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
