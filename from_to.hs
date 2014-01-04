{-# LANGUAGE OverloadedStrings #-}
import TPG.WebAPI
import TPG.Structured
import System.Directory
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BS
import Cfg
import Control.Monad
import Control.Monad.Loops
import Data.Either

getDepartureList :: String -> [String] -> IO [Departure]
getDepartureList key stopCodes = do
  thisNextDepartures <- forkMapM (getNextDepartures key) stopCodes
  let successfulNexts = rights thisNextDepartures
  let ndList nd = case nd of
        Nothing -> []
        Just dpts -> (departures dpts)
  let mapped = map ndList successfulNexts
  return (join mapped)

getThermometerList :: String -> [String] -> IO [Thermometer]
getThermometerList key departureCodes = do
  thisThermometers <- forkMapM (getThermometer key) departureCodes
  let successfulThermometers = rights thisThermometers
  let tList t = case t of
        Nothing -> []
        Just therm -> [therm]
  let mapped = map tList successfulThermometers
  return (join mapped)

calculate_route :: String -> [String] -> [String] -> Int -> IO ()
calculate_route key fromStopCodeList toStopCodeList maxIter = do
  putStrLn (show fromStopCodeList)
  dList <- getDepartureList key fromStopCodeList
  let departureCodes = map (show . departureCode) dList
  putStrLn (show departureCodes)
  thermometers <- getThermometerList key departureCodes
  let rds = map reachableDestinationCodes thermometers
  let destinationIntersections =
        [ (tdc,th) | p <- rds, (tdc,th) <- p, dc <- toStopCodeList, dc == tdc]
  putStrLn (show destinationIntersections)
  putStrLn (show toStopCodeList)

calculate_route_with_names :: String -> String -> String -> IO ()
calculate_route_with_names key fromStopName toStopName = do
  mFromStop <- getStops key fromStopName
  mToStop <- getStops key toStopName
  case (mFromStop,mToStop) of
    (Just fromStop,Just toStop) -> do
      let fromStopCodeList = stopCodeList fromStop
      let toStopCodeList = stopCodeList toStop
      calculate_route key fromStopCodeList toStopCodeList 5 -- max 5 changes
    _ -> putStrLn "Could not match from/to"

main = do
  args <- getArgs
  if length args < 2 then
    do
      putStrLn "Usage: from_to fromStationName toStationName"
  else
    do
      home_directory <- getHomeDirectory
      config_handle <- openFile (home_directory ++ "/.tpg_tests") ReadMode
      contents <- BS.hGetContents config_handle
      let key = getApiKeyFromConfigString contents
      case key of
        Nothing -> error "Did not find API key"
        Just key -> calculate_route_with_names key (head args) (head (tail args))
      hClose config_handle

