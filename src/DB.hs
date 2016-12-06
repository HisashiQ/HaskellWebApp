module DB (dbConnect,initialiseDB,insertDB) where 

import Database.HDBC
import Database.HDBC.Sqlite3
import Type

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
   	  run conn "CREATE TABLE events (date DATE, time TIME, place VARCHAR(40), magnitude DECIMAL, latitude DECIMAL, longitude DECIMAL, depth DECIMAL)" []
      commit conn
      putStrLn "Database initialised!"
   else
      return ()

insertDB :: Earthquake -> IO ()  --Assuming we have a data type called "Earthquake"
insertDB event = do
   conn <- dbConnect
   let query = "INSERT INTO events VALUES (?,?,?,?,?,?,?)"
   let record = [ toSql.date $ event   --Assuming Earthquake has variables called date, time, etc...
                , toSql.time $ event
                , toSql.place $ event
                , toSql.magnitude $ event
                , toSql.latitude $ event
                , toSql.longitude $ event
                , toSql.depth $ event
                ]
   run conn query record 
   commit conn
   disconnect conn

getFromDB :: DBQuery -> IO [[SqlValue]] --Assuming we have a data type called "DBQuery"
getFromDB request = do
	conn <- dbConnect
	--Get all earthquakes in a given date range
	if (requestType request) == "date" then
		let query = "SELECT * FROM events WHERE date >= " ++ from ++ " AND date <= " ++ until
		    where
		    	from = startDate request
		    	until = endDate request
		else if a then b --What other query? Earthquakes in a given lat-long rectangle? Earthquakes on a given continent?
    quickQuery conn query
    disconnect conn



	

