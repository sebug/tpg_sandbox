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

getAllNextDepartures :: String -> String -> String -> String -> IO String
getAllNextDepartures key stopCode lineCode destinationCode =
  rb (((apiCall key "GetAllNextDepartures" `withArg` "stopCode" $ stopCode) `withArg` "lineCode" $ lineCode) `withArg` "destinationCode" $ destinationCode)

getThermometer :: String -> String -> IO String
getThermometer key =
  rb . (apiCall key "GetThermometer" `withArg` "departureCode")

getThermometerPhysicalStops :: String -> String -> IO String
getThermometerPhysicalStops key =
  rb . (apiCall key "GetThermometerPhysicalStops" `withArg` "departureCode")

getLinesColors :: String -> IO String
getLinesColors key =
  rb (apiCall key "GetLinesColors")

getDisruptions :: String -> IO String
getDisruptions key =
  rb (apiCall key "GetDisruptions")
