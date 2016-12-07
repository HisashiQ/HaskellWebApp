module Main where

import DownloadData_x
import Parser_x
import System.IO

main = do 
     d <- downloadURL "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-10-01&endtime=2016-12-01"
     mapM_ putStrLn (getFeatures (head (splitData (fromRight d))))
