{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import JsonType

-- instance ToJSON Properties
-- instance FromJSON Geometry
-- instance ToJSON Geometry
-- instance FromJSON Coordinates
-- instance ToJSON Coordinates
-- -- | Location of the local copy, in case you have it,
-- --   of the JSON file.
-- jsonFile :: FilePath
-- jsonFile = "pizza.json"

-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "http://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=2016-10-01"

-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--
-- Read the local copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
--}

{--}
-- Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL
--}

main :: IO ()
main = do
 -- Get JSON data and decode it
 -- d <- (eitherDecode <$> getJSON) :: IO (Either String [Earthquake])
 d <- getJSON
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 -- case d of
 --  Left err -> putStrLn err
 --  Right ps -> print "It works"
 print d
