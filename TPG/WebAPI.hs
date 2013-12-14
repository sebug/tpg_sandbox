module TPG.WebAPI
(
getStops
, getNextDepartures
, getAllNextDepartures
, getThermometer
, getThermometerPhysicalStops
, getLinesColors
, getDisruptions
) where

import TPG.WebAPI.Bases
import Network.HTTP

rb :: String -> IO String
rb url = simpleHTTP (getRequest url) >>= getResponseBody

getStops :: String -> String -> IO String
getStops key =
  rb . (apiCall key "GetStops" `withArg` "stopName")

getNextDepartures :: String -> String -> IO String
getNextDepartures key =
  rb . (apiCall key "GetNextDepartures" `withArg` "stopCode")

getAllNextDepartures :: String -> String -> IO String
getAllNextDepartures key =
  rb . (apiCall key "GetAllNextDepartures" `withArg` "stopCode")

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
