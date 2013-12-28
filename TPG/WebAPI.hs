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
getStops key stopCode = do
  text <- rb ((apiCall key "GetStops" `withArg` "stopName") stopCode)
  return (parseStops text)

--getPhysicalStops :: String -> String -> IO String
--getPhysicalStops key =
--  rb . (apiCall key "GetPhysicalStops" `withArg` "stopCode")

getNextDepartures :: String -> String -> IO (Maybe NextDepartures)
getNextDepartures key stopCode = do
  text <- rb ((apiCall key "GetNextDepartures" `withArg` "stopCode") stopCode)
  return (parseNextDepartures text)

getAllNextDepartures :: String -> String -> String -> String -> IO (Maybe NextDepartures)
getAllNextDepartures key stopCode lineCode destinationCode = do
  text <- rb (((apiCall key "GetAllNextDepartures" `withArg` "stopCode" $ stopCode) `withArg` "lineCode" $ lineCode) `withArg` "destinationCode" $ destinationCode)
  return (parseNextDepartures text)

getThermometer :: String -> String -> IO (Maybe Thermometer)
getThermometer key departureCode = do
  text <- rb (apiCall key "GetThermometer" `withArg` "departureCode" $ departureCode)
  return (parseThermometer text)

getThermometerPhysicalStops :: String -> String -> IO (Maybe ThermometerPhysicalStops)
getThermometerPhysicalStops key departureCode = do
  text <- rb (apiCall key "GetThermometerPhysicalStops" `withArg` "departureCode" $ departureCode)
  return (parseThermometerPhysicalStops text)

getLinesColors :: String -> IO (Maybe LineColors)
getLinesColors key = do
  text <- rb (apiCall key "GetLinesColors")
  return (parseLineColors text)

getDisruptions :: String -> IO (Maybe Disruptions)
getDisruptions key = do
  text <- rb (apiCall key "GetDisruptions")
  return (parseDisruptions text)

