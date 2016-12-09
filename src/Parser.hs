module Parser where

import Data.List.Split
import Data.List
import Data.UnixTime as Unix
import System.Time
import Text.Read 
import Data.Maybe


--Takes the result of the http download and produces a list of raw earthquake data strings
getEarthquakes :: Either String String -> [String]
getEarthquakes (Left _) = ["Parsing error"]
getEarthquakes (Right x) = init $ tail (splitOn "\"type\":\"Feature\"" x)

--Takes a raw earthquake string and returns a property value
getProperty :: String -> String -> String
getProperty a b = head $ splitOn ",\"" $ last $ splitOn property b
    where property = "\"" ++ a ++ "\":"

--Takes a raw earthquake string and returns a list of floats showing coordinates
getCoordinates :: String -> [Maybe Double]
getCoordinates x = map readMaybe $ splitOn "," (init.init.tail $ getProperty "coordinates" x)

getUTC x = toUTCTime (Unix.toClockTime (fromEpochTime x))

getYear Nothing = Nothing
getYear (Just x) = Just (ctYear (getUTC x))


getMonth Nothing = Nothing
getMonth (Just x) = Just (monthInt (getUTC x))
  where monthInt x
           | ctMonth x == January = 01
           | ctMonth x == February = 02
           | ctMonth x == March = 03
           | ctMonth x == April = 04
           | ctMonth x == May = 05
           | ctMonth x == June = 06
           | ctMonth x == July = 07
           | ctMonth x == August = 08
           | ctMonth x == September = 09
           | ctMonth x == October = 10
           | ctMonth x == November = 11
           | ctMonth x == December = 12

getDay Nothing = Nothing
getDay (Just x) = Just (ctDay (getUTC x))


--Checks if want date or magnitude
dateOrMag :: String -> IO (String)
dateOrMag line
	     | (line == "1") = do (return "date")
	     | (line == "2") = do (return "time")
	     | otherwise = do
	     	putStrLn "Error, please enter again"
	     	newline <- getLine
	     	dateOrMag newline
