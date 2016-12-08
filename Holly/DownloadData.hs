module DownloadData where

import System.IO
import Network.HTTP
import Network.URI
import Database.HDBC
import Data.Maybe
import Data.Either

downloadURL :: String -> IO (Either String String)
downloadURL url =
    do response <- simpleHTTP request
       case response of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [],rqBody = ""}
          uri = fromJust $ parseURI url
