module Types where

import Parser_x

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

makeEarthquake x = Earthquake { year = getYear $ (read $ take 10 $ getProperty "time" x)
                              , month = getMonth $ (read $ take 10 $ getProperty "time" x)
                              , day = getDay $ (read $ take 10 $ getProperty "time" x)
                              , place = init.tail $ getProperty "place" x
                              , magnitude = read $ getProperty "mag" x
                              , longitude = (getCoordinates x) !! 1
                              , latitude = (getCoordinates x) !! 0
                              , depth = (getCoordinates x) !! 2
                              , url = init.tail $ getProperty "url" x
                              }
