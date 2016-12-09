module Types where

import Parser

data Earthquake = Earthquake { year :: Int
                             , month :: Int
                             , day :: Int
                             , place :: String
                             , magnitude :: Double
                             , longitude :: Double
                             , latitude :: Double
                             , depth :: Double
                             , url :: String
                             } deriving Show

makeEarthquake :: String -> Earthquake
makeEarthquake x = Earthquake { year = getYear $ (read $ take 10 $ getProperty "time" x)
                              , month = getMonth $ (read $ take 10 $ getProperty "time" x)
                              , day = getDay $ (read $ take 10 $ getProperty "time" x)
                              , place = init.tail $ getProperty "place" x
                              , magnitude = read $ getProperty "mag" x
                              , longitude = (getCoordinates x) !! 0
                              , latitude = (getCoordinates x) !! 1
                              , depth = (getCoordinates x) !! 2
                              , url = init.tail $ getProperty "url" x
                              }

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
