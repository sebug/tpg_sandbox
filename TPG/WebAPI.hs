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

rb :: String -> IO String
rb url = simpleHTTP (getRequest url) >>= getResponseBody

getStops :: String -> String -> IO (Maybe Stops)
getStops key stopCode =
  rb ((apiCall key "GetStops" `withArg` "stopName") stopCode)
  >>= return . parseStops

--getPhysicalStops :: String -> String -> IO String
--getPhysicalStops key =
--  rb . (apiCall key "GetPhysicalStops" `withArg` "stopCode")

getNextDepartures :: String -> String -> IO (Maybe NextDepartures)
getNextDepartures key stopCode =
  rb ((apiCall key "GetNextDepartures" `withArg` "stopCode") stopCode)
  >>= return . parseNextDepartures

getAllNextDepartures :: String -> String -> String -> String -> IO (Maybe NextDepartures)
getAllNextDepartures key stopCode lineCode destinationCode =
  rb (((apiCall key "GetAllNextDepartures" `withArg` "stopCode" $ stopCode) `withArg` "lineCode" $ lineCode) `withArg` "destinationCode" $ destinationCode)
  >>= return . parseNextDepartures

getThermometer :: String -> String -> IO (Maybe Thermometer)
getThermometer key departureCode =
  rb (apiCall key "GetThermometer" `withArg` "departureCode" $ departureCode)
  >>= return . parseThermometer

getThermometerPhysicalStops :: String -> String -> IO (Maybe ThermometerPhysicalStops)
getThermometerPhysicalStops key departureCode =
  rb (apiCall key "GetThermometerPhysicalStops" `withArg` "departureCode" $ departureCode) >>= return . parseThermometerPhysicalStops

getLinesColors :: String -> IO (Maybe LineColors)
getLinesColors key =
  rb (apiCall key "GetLinesColors") >>= return . parseLineColors

getDisruptions :: String -> IO (Maybe Disruptions)
getDisruptions key =
  rb (apiCall key "GetDisruptions") >>= return . parseDisruptions


