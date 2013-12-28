module TPG.WebAPI
(
getStops
--, getPhysicalStops
, getNextDepartures
, getAllNextDepartures
, getThermometer
, getThermometerPhysicalStops
, getLinesColors
, getDisruptions
) where

import TPG.WebAPI.Bases
import Network.HTTP
import TPG.Structured
import Control.Monad

rb :: String -> IO String
rb url = simpleHTTP (getRequest url) >>= getResponseBody

getStops :: String -> String -> IO (Maybe Stops)
getStops key stopCode =
  liftM parseStops $ rb $ apiCall key "GetStops" `withArg` "stopName" $ stopCode

--getPhysicalStops :: String -> String -> IO String
--getPhysicalStops key =
--  rb . (apiCall key "GetPhysicalStops" `withArg` "stopCode")

getNextDepartures :: String -> String -> IO (Maybe NextDepartures)
getNextDepartures key stopCode =
  liftM parseNextDepartures $ rb $ apiCall key "GetNextDepartures" `withArg` "stopCode" $ stopCode

getAllNextDepartures :: String -> String -> String -> String -> IO (Maybe NextDepartures)
getAllNextDepartures key stopCode lineCode destinationCode =
  liftM parseNextDepartures $ rb $ (apiCall key "GetAllNextDepartures" `withArg` "stopCode" `andArg` "lineCode" `andArg2` "destinationCode") stopCode lineCode destinationCode

getThermometer :: String -> String -> IO (Maybe Thermometer)
getThermometer key departureCode =
  liftM parseThermometer $ rb $ apiCall key "GetThermometer" `withArg` "departureCode" $ departureCode

getThermometerPhysicalStops :: String -> String -> IO (Maybe ThermometerPhysicalStops)
getThermometerPhysicalStops key departureCode =
  liftM parseThermometerPhysicalStops $ rb $ apiCall key "GetThermometerPhysicalStops" `withArg` "departureCode" $ departureCode

getLinesColors :: String -> IO (Maybe LineColors)
getLinesColors key =
  liftM parseLineColors $ rb $ apiCall key "GetLinesColors"

getDisruptions :: String -> IO (Maybe Disruptions)
getDisruptions key =
  liftM parseDisruptions $ rb $ apiCall key "GetDisruptions"


