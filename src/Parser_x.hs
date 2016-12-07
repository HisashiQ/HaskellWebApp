module Parser_x where

import Data.List.Split

splitData :: [Char] -> [[Char]]
splitData x = init $ tail (splitOn "\"type\":\"Feature\"" x)

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

getFeatures :: String -> [String]
getFeatures x = splitOn "," x

