module Types where

import Parser
import Data.Maybe 
import Text.Read

-- | Data type Earthquake holds all of the information about a single earthquake event
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

-- | Take a single raw json string and return an earthquake, using the makeEarthquake function
getEarthquakeData :: String -> Earthquake
getEarthquakeData rawString = makeEarthquake rawString rawTime coordinates
    where
      rawTime = take 10 $ getProperty "time" rawString
      coordinates = getCoordinates rawString

-- | Takes a whole raw string, the extracted time and the extracted coordinates, and parses into an Earthquake
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

-- | Take an earthquake and check it has been correctly parsed. Unparsable Json will contain a "Nothing".
validateEarthquake :: Earthquake -> Bool
validateEarthquake x
    | elem Nothing [year x, month x, day x] || elem Nothing [magnitude x, longitude x, latitude x, depth x] == True = False
    | otherwise = True

-- | Data type region holds the name and boundaries of one of the world's continents
data Region = Region { name :: String
                     , latFrom :: Double
                     , latTo :: Double
                     , longFrom :: Double
                     , longTo :: Double
                     } deriving Show

-- | Make a region from a list of given strings
makeRegion :: [String] -> Region
makeRegion xs = Region { name = ( xs !! 0)
                       , latFrom = ( read $ xs !! 1 )
                       , latTo = ( read $ xs !! 2)
                       , longFrom = ( read $ xs !! 3)
                       , longTo = ( read $ xs !! 4)
                       }
