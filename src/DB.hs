module DB where 

import Database.HDBC
import Database.HDBC.Sqlite3
import Types

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
    run conn "CREATE TABLE events (time DECIMAL, place VARCHAR(40), magnitude DECIMAL, latitude DECIMAL, longitude DECIMAL, depth DECIMAL, url VARCHAR(40))" []
    commit conn
    putStrLn "Database initialised!"
  else
    return ()

insertDB :: Earthquake -> IO ()  --Assuming we have a data type called "Earthquake"
insertDB event = do
   conn <- dbConnect
   let query = "INSERT INTO events VALUES (?,?,?,?,?,?,?)"
   let record = [ toSql.time $ event   --Assuming Earthquake has variables called date, time, etc...
                , toSql.place $ event
                , toSql.magnitude $ event
                , toSql.latitude $ event
                , toSql.longitude $ event
                , toSql.depth $ event
                , toSql.url $ event
                ]
   run conn query record 
   commit conn
   disconnect conn

getWholeDB :: IO [[SqlValue]]
getWholeDB = do
  conn <- dbConnect
  let query = "SELECT * FROM events"
  quickQuery' conn query []

getDbContentsAsList :: [[SqlValue]] -> [[String]]
getDbContentsAsList x = map (map fromSql) x


{--getFromDB :: DBQuery -> IO [[SqlValue]] --Assuming we have a data type called "DBQuery"
getFromDB request = do
	conn <- dbConnect
	--Get all earthquakes in a given date range
	if (requestType request) == "date" then
		let query = "SELECT * FROM events WHERE date >= " ++ (startDate request) ++ " AND date <= " ++ (endDate request)
	else if a then b --What other query? Earthquakes in a given lat-long rectangle? Earthquakes on a given continent?
    quickQuery conn query
    disconnect conn--}




	

