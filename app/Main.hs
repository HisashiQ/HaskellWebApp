module Main where

import Database.HDBC
import DownloadData_x
import Parser_x
import System.IO
import Types
import DB

main = do 
     d <- downloadURL "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-12-01&endtime=2016-12-02"
     let earthquakeStrings = getEarthquakes d
     let earthquakeData = map makeEarthquake earthquakeStrings

     dbConnect
     initialiseDB
     insertDB (earthquakeData !! 10)
     insertDB (earthquakeData !! 11)

     db <- getWholeDB
     let dbContents = getDbContentsAsList db
     mapM_ print $ dbContents



 