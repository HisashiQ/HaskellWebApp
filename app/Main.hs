module Main where

import Database.HDBC
import DownloadData_x
import Parser_x
import System.IO
import Types
import DB
import RegionsDB

main = do

--Checks if want date or magnitude
dateOrMag :: String -> IO (String)
dateOrMag line
    | (line == "1") = do (return "date")
    | (line == "2") = do (return "time")
    | otherwise = do
        putStrLn "Error, please enter again"
        newline <- getLine
        dateOrMag newline

--Download json
     d <- downloadURL "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-12-01&endtime=2016-12-02"
     let earthquakeStrings = getEarthquakes d
     let earthquakeData = map makeEarthquake earthquakeStrings

     --Create db with events table
     initialiseDB
     --insert earthquakes
     mapM_ insertDB earthquakeData


     --Add regions table to db
     createRegionsTable
     --insert regions
     let regions = createRegions
     mapM_ insertRegion regions

     putStrLn "Enter 1 for query by date or 2 for query by minimum magnitude"
     line <- getLine
     let decision = dateOrMag line


     db <- getWholeDB
     let dbContents = getDbContentsAsList db
     mapM_ print $ dbContents


     let userSelection = "Asia"

     --get selected region from database and create region
     regionDb <- getFromDB $ "regions WHERE region == \"" ++ userSelection ++ "\""
     --regionDb <- getFromDB $ "regions"
     let r = map makeRegion $ getDbContentsAsList regionDb

     --get lat/long values as strings
     let a = show $ latFrom $ head r
     let b = show $ latTo $ head r
     let c = show $ longFrom $ head r
     let d = show $ longTo $ head r


     --get earthquakes in given region
     matchingEarthquakes <- getFromDB $ "events WHERE latitude >= " ++ a ++ " AND latitude < " ++ b ++ " AND longitude >= " ++ c ++ " AND longitude < " ++ d
     --matchingEarthquakes <- getFromDB
     --print out matching earthquakes
     mapM_ print $ getDbContentsAsList matchingEarthquakes
