module DB where

import Database.HDBC
import Database.HDBC.Sqlite3
import Types
import System.Directory
import Control.Monad

deleteOldDb :: IO ()
deleteOldDb = do
	exists <- doesFileExist "earthquakes.db"
	when exists (removeFile "earthquakes.db")

dbConnect :: IO Connection
dbConnect = do
   conn <- connectSqlite3 "earthquakes.db"
   return conn

-- | Initialises the database creating tables

initialiseDB :: IO ()
initialiseDB = do
  conn <- dbConnect
  tables <- getTables conn
  if not (elem "events" tables) then do
    run conn "CREATE TABLE events (year INTEGER, month SMALLINT, day SMALLINT, place VARCHAR(40), magnitude DECIMAL, latitude DECIMAL, longitude DECIMAL, depth DECIMAL, url VARCHAR(40))" []
    commit conn
    putStrLn "Database initialised!"
  else
    return ()

insertDB :: [Earthquake] -> IO ()
insertDB events = do
  let validEvents = filter validateEarthquake events
  conn <- dbConnect
  mapM_ (buildRecord conn) validEvents
  commit conn
  disconnect conn

buildRecord :: IConnection conn => conn -> Earthquake -> IO Integer
buildRecord conn event = do
    let query = "INSERT INTO events VALUES (?,?,?,?,?,?,?,?,?)"
    let record = [ toSql.year $ event
                 , toSql.month $ event
                 , toSql.day $ event
                 , toSql.place $ event
                 , toSql.magnitude $ event
                 , toSql.latitude $ event
                 , toSql.longitude $ event
                 , toSql.depth $ event
                 , toSql.url $ event
                 ]       
    run conn query record												        

    
getFromDB :: String -> IO [[SqlValue]]
getFromDB xs = do
  conn <- dbConnect
  let query = "SELECT * FROM " ++ xs
  quickQuery' conn query []

-- getFromDB :: IO [[SqlValue]]
-- getFromDB = do
--   conn <- dbConnect
--   let query = "SELECT * FROM events"
--   quickQuery' conn query []


getDbContentsAsList :: [[SqlValue]] -> [[String]]
getDbContentsAsList x = map (map fromSql) x


--Creates Json and concatinates magnitude, url etc to the json object.
-- Create Json is called recursivley
createJson [] [] [] [] [] [] [] [] [] = ""
createJson (x:xs) (k:ks) (b:bs) (y:ys) (z:zs) (a:as) (yr:yrs) (m:ms) (d:ds) = "{\"type\":\"Feature\",\"properties\":{\"mag\":" ++ x ++ ",\"place\":\"" ++ k ++ "\",\"time\":\"" ++ d ++ "-" ++ m ++ "-" ++ yr ++ "\",\"tz\":480,\"url\":\"" ++ b ++ "\",\"felt\":2,\"cdi\":3.4,\"mmi\":null,\"alert\":null,\"status\":\"REVIEWED\",\"tsunami\":null,\"sig\":\"449\",\"net\":\"us\",\"code\":\"c000csx3\",\"ids\":\",usc000csx3,\",\"sources\":\",us,\",\"types\":\",dyfi,eq-location-map,general-link,geoserve,historical-moment-tensor-map,historical-seismicity-map,nearby-cities,origin,p-wave-travel-times,phase-data,scitech-link,tectonic-summary,\"},\"geometry\":{\"type\":\"Point\",\"coordinates\":[" ++ y ++ "," ++ z ++ "," ++ a ++ "]},\"id\":\"usc000csx3\"}" ++ "," ++ (createJson xs ks bs ys zs as yrs ms ds)

getMag x = x !! 4
getUrl x =  x !! 8
getLat x = x !! 6
getLong x = x !! 5
getDepth x = x !! 7
getPlace x = x !! 3
getYear x = x !! 0
getMonth x = x !! 1
getDay x = x !! 2

getFullJson a = createJson (map getMag a) (map getPlace a) (map getUrl a) (map getLat a) (map getLong a) (map getDepth a) (map getYear a) (map getMonth a) (map getDay a)

{--getFromDB :: DBQuery -> IO [[SqlValue]] --Assuming we have a data type called "DBQuery"
getFromDB request = do
	conn <- dbConnect
	--Get all earthquakes in a given date range
	if (requestType request) == "date" then
		let query = "SELECT * FROM events WHERE date >= " ++ (startDate request) ++ " AND date <= " ++ (endDate request)
	else if a then b --What other query? Earthquakes in a given lat-long rectangle? Earthquakes on a given continent?
    quickQuery conn query
    disconnect conn--}
