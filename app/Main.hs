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
     let listOfInfo = getDbContentsAsList matchingEarthquakes

     writeFile "earthquakeMap.json" "eqfeed_callback({'type':'FeatureCollection','features':["
     appendFile "earthquakeMap.json" (getFullJson listOfInfo)
     appendFile "earthquakeMap.json" "]});"

    --  f <- createProcess (proc "open /Users/quinn/Code/Haskell/HaskellEarthquakeMapper/index.html" [])
    --  r <- createProcess (proc "rm /Users/quinn/Code/Haskell/HaskellEarthquakeMapper/earthquakes.db" [])
     f <- createProcess (shell "rm /Users/quinn/Code/Haskell/HaskellEarthquakeMapper/earthquakes.db")
     r <- createProcess (shell "open index.html")
     putStrLn "Opening browser"
