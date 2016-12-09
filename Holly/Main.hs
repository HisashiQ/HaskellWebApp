module Main where

import Database.HDBC
import DownloadData_x
import Parser_x
import System.IO
import Types
import DB

--Checks if want date or magnitude
dateOrMag :: String -> IO (String)
dateOrMag line
    | (line == "1") = do (return "date")
    | (line == "2") = do (return "time")
    | otherwise = do
        putStrLn "Error, please enter again"
        newline <- getLine
        dateOrMag newline

main = do 
     d <- downloadURL "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-12-01&endtime=2016-12-02"
     let earthquakeStrings = getEarthquakes d
     let earthquakeData = map makeEarthquake earthquakeStrings

     dbConnect
     initialiseDB
     insertDB (earthquakeData !! 10)
     insertDB (earthquakeData !! 11)

     putStrLn "Enter 1 for query by date or 2 for query by minimum magnitude"
     line <- getLine
     let decision = dateOrMag line


     db <- getWholeDB
     let dbContents = getDbContentsAsList db
     mapM_ print $ dbContents



 