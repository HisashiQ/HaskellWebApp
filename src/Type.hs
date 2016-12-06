--This is the "Type" file from lab7, placeholder to be modified.
module Type ( Price
            , stock
            , date
            , open
            , high
            , low
            , close
            , volume
            , adjClose
            , readPrice
            ) where 

import Data.List
import Debug.Trace

data Price = Price { stock :: String
                   , date :: String
                   , open :: Double
                   , high :: Double
                   , low :: Double
                   , close :: Double
                   , volume :: Int
                   , adjClose :: Double
                   } deriving (Show,Eq,Read)

breakAll :: Char -> String -> [String]
breakAll c line | elem c line = x : (breakAll c (tail xs))
                | otherwise = [line]
   where
      (x,xs) = break (==c) line

-- read line such as
-- "2016-11-16,40.099998,41.049999,40.099998,40.98,15592700,40.98"
readPrice :: String -> String -> Price
readPrice stock line = 
   Price { stock = stock
         , date = tokens !! 0
         , open = read (tokens !! 1) :: Double
         , high = read (tokens !! 2) :: Double
         , low = read (tokens !! 3) :: Double
         , close = read (tokens !! 4) :: Double
         , volume = read (tokens !! 5) :: Int
         , adjClose = read (tokens !! 6) :: Double
         }
   where
      tokens = breakAll ',' line
