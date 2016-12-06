--This is the "download" file from lab7. As a placeholder to be modified for our own program

module Download (downloadURL) where 

import Network.URI
import Network.HTTP
import Data.Maybe
import Data.Either

{- | Given a valid URI, this function will attempt to download the page contents. -}
downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x -> return $ Left ("Error connecting: " ++ show x)
        Right r -> case rspCode r of
                (2,_,_) -> return $ Right (rspBody r)
                (3,_,_) -> -- A HTTP redirect
                    case findHeader HdrLocation r of
                        Nothing -> return $ Left (show r)
                        Just url -> downloadURL url
                _ -> return $ Left (show r)
    where
        request = Request {
            rqURI = uri,
            rqMethod = GET,
            rqHeaders = [],
            rqBody = ""}
        uri = fromJust $ parseURI url