module Main where

import System.Environment   
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text as T

-- |This is the main function
main = do 
    args <- getArgs
    checkArgs args
    let endpoint = "https://api.forecast.io/forecast/"
    let apiKey   = "" -- Insert your API key here
    let lat      = "38.035556"
    let lon      = "-78.503611"
    simpleHttp (endpoint++apiKey++"/"++lat++","++lon) >>= (L.writeFile "forecast.txt")
    forecastData <- L.readFile "forecast.txt"
    let forecast = decode forecastData :: Maybe Forecast
    mapM (processArg forecast) args
    putStrLn "Powered by Forecast.io"

-- |Print out how to get the weather if no command-line arguments are provided
checkArgs :: [String] -> IO ()
checkArgs [] = putStrLn "Use -w to get the weather!"
checkArgs other = putStrLn ""

-- |Process each command-line argument
processArg :: Maybe Forecast -> String -> IO ()
processArg (Just (Forecast {currently = current})) "-current"              = putStrLn ("Current weather: " ++ (show (summary current)) ++ ", " ++ (show (temperature current)) ++ "F")
processArg (Just (Forecast {latitude = lat, longitude = lon})) "-location" = putStrLn ("Location: " ++ (show lat) ++ ", " ++ (show lon))
processArg (Just (Forecast {latitude = lat, longitude = lon})) other       = putStrLn ("Use -current to get the current weather at: " ++ (show lat) ++ ", " ++ (show lon) ++ "!")
processArg badforecast other = putStrLn "Forecasts broken."

-- |The currently object
data Currently = Currently
	{ summary     :: T.Text
    , temperature :: Float
	} deriving Show

-- |The forecast object
data Forecast = Forecast
    { latitude  :: Float
    , longitude :: Float
    , timezone  :: T.Text
	, currently :: Currently
    } deriving Show

{-# LANGUAGE OverloadedStrings #-}
-- |Code to create forecast object from JSON
instance FromJSON Currently where
    parseJSON (Object v) = Currently
                           <$> v .: (T.pack "summary")
                           <*> v .: (T.pack "temperature")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

{-# LANGUAGE OverloadedStrings #-}
-- |Code to create forecast object from JSON
instance FromJSON Forecast where
    parseJSON (Object v) = Forecast
                           <$> v .: (T.pack "latitude")
                           <*> v .: (T.pack "longitude")
                           <*> v .: (T.pack "timezone")
						   <*> v .: (T.pack "currently")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero