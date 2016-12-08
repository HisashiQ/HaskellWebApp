module Types where

import Parser_x

data Earthquake = Earthquake { time :: Integer
                             , place :: String
                             , magnitude :: Double
                             , longitude :: Double
                             , latitude :: Double
                             , depth :: Double
                             , url :: String
                             } deriving Show

makeEarthquake :: String -> Earthquake
makeEarthquake x = Earthquake { time = read $ getProperty "time" x
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


