module Main where

import Database.HDBC
import DownloadData
import Parser
import System.IO
import Types
import DB
import RegionsDB
import System.Process

main = do

     -- Download json
     d <- downloadURL "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-10-01&endtime=2016-12-01"
     let earthquakeStrings = getEarthquakes d
     let earthquakeData = map getEarthquakeData earthquakeStrings
     
     -- Delete the previous json we created
     deleteOldDb

     -- Create database with events table
     initialiseDB
     -- Insert earthquakes into database
     insertDB earthquakeData


     -- Add regions table to database
     createRegionsTable
     -- Insert regions
     let regions = createRegions
     mapM_ insertRegion regions

     -- Ask user to query by region, date or magnitude
     putStrLn "Enter 1 for query by date, 2 for query by minimum magnitude or 3 for query by region"
     line <- getLine
     -- Process the input and return the map
     dateMagOrRegion line >>= processUserInput


-- | Takes the user input of 1, 2 or 3 and returns date, magnitude or region
dateMagOrRegion :: String -> IO (String)
dateMagOrRegion line
    | (line == "1") = do (return "date")
    | (line == "2") = do (return "magnitude")
    | (line == "3") = do (return "region")
    | otherwise = do
        putStrLn "Error, please enter again"
        newline <- getLine
        dateMagOrRegion newline

-- | Processes date, magnitude or region as string and takes more specific input. Returns an IO action and catches input errors
processUserInput :: String -> IO()
processUserInput x
      |(x == "date") = do 
            putStrLn "Enter a day (up to 31)"
            input1 <- getLine
            let intInput1 = read input1 :: Int
            putStrLn "Enter a month: 1 for October, 2 for November"
            input2 <- getLine
            processDate intInput1 input2
            return ()
      |(x == "magnitude") = do
            putStrLn "Enter a minimum magnitude from the following code:"
            putStrLn "1 = Minor (3 and up)"
            putStrLn "2 = Light (4 and up)"
            putStrLn "3 = Moderate (5 and up)"
            putStrLn "4 = Strong (6 and up)"
            putStrLn "5 = Major (7 and up)"
            putStrLn "6 = Great (8 and up)"
            input <- getLine
            processMagnitude input
            return ()
      |(x == "region") = do
            putStrLn "Enter a region by number:"
            putStrLn "1 = North America"
            putStrLn "2 = South America"
            putStrLn "3 = Africa"
            putStrLn "4 = Europe"
            putStrLn "5 = Asia"
            putStrLn "6 = Australasia"
            input <- getLine
            processRegion input
            return ()

-- | Processes date information given and queries the database. Catches any errors
processDate :: Int -> String -> IO ()
processDate day month
  | (day == 31) = if (month == "1") then
                     do (callDateDB day 10)
                  else
                    do
                      putStrLn "Incorrect date. Please try again"
                      processUserInput "date"
                      return ()
  | (day<32)&&(day>0)&&(month=="1") = do (callDateDB day 10)
  | (day<32)&&(day>0)&&(month=="2") = do (callDateDB day 11)
  | otherwise = do
                 putStrLn "Incorrect date. Please try again"
                 processUserInput "date"
                 return ()

-- | Processes magnitude information given and queries the database. Catches any errors
processMagnitude :: String -> IO ()
processMagnitude x
  | (x == "1") = do
                  callMagnitudeDB "3"
  | (x == "2") = do 
                  callMagnitudeDB "4"
  | (x == "3") = do
                  callMagnitudeDB "5"
  | (x == "4") = do
                  callMagnitudeDB "6"
  | (x == "5") = do
                  callMagnitudeDB "7"
  | (x == "6") = do
                  callMagnitudeDB "8"
  | otherwise = do
  	              putStrLn "error"

-- | Processes region information given and queries the database. Catches any errors
processRegion :: String -> IO ()
processRegion x
  | (x=="1") = do
                callRegionDB "N. America"
  | (x=="2") = do
                callRegionDB "S. America"
  | (x=="3") = do 
                callRegionDB "Africa"
  | (x=="4") = do
                callRegionDB "Europe"
  | (x=="5") = do
                callRegionDB "Asia"
  | (x=="6") = do
                callRegionDB "Australasia"
  | otherwise = do
        putStrLn "Error, please enter again"
        processUserInput "region"
        return ()

-- | Query the database by date
callDateDB :: Int -> Int -> IO ()
callDateDB day month = do
    matchingEarthquakes <- getFromDB $ "events WHERE day >= " ++ (show day) ++ " AND month >= " ++ (show month)
    displayMap matchingEarthquakes

-- | Query the database by magnitude
callMagnitudeDB :: String -> IO ()
callMagnitudeDB x = do
    matchingEarthquakes <- getFromDB $ "events WHERE magnitude >= "  ++ x
    displayMap matchingEarthquakes

-- | Query the database by region
callRegionDB :: String -> IO ()
callRegionDB x = do
    -- get selected region from database and create region
    regionDb <- getFromDB $ "regions WHERE region == \"" ++ x ++ "\""
    let r = map makeRegion $ getDbContentsAsList regionDb
     --get lat/long values as strings
    let a = show $ latFrom $ head r
    let b = show $ latTo $ head r
    let c = show $ longFrom $ head r
    let d = show $ longTo $ head r
    --get earthquakes in given region
    matchingEarthquakes <- getFromDB $ "events WHERE latitude >= " ++ a ++ " AND latitude < " ++ b ++ " AND longitude >= " ++ c ++ " AND longitude < " ++ d
    displayMap matchingEarthquakes

-- | Display a map of queried data in browser
displayMap :: [[SqlValue]] -> IO ()
displayMap x = do
	let listOfInfo = getDbContentsAsList x
        writeFile "earthquakeMap.json" "eqfeed_callback({'type':'FeatureCollection','features':["
        appendFile "earthquakeMap.json" (getFullJson listOfInfo)
        appendFile "earthquakeMap.json" "]});"
        r <- createProcess (shell "open index.html")
        putStrLn "Opening browser"
