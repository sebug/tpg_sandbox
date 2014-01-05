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

getDepartureList :: String -> [([String],[Thermometer])] -> IO [(Departure,[String],[Thermometer])]
getDepartureList key stopCodesPaired = do
  thisNextDepartures <- forkMapM (\p -> do
                                     nd <- getNextDepartures key ((head . fst) p)
                                     return (nd, fst p, snd p)) stopCodesPaired
  let successfulNexts = rights thisNextDepartures
  let ndList nd = case nd of
        (Nothing,_,_) -> []
        (Just dpts,sts,ts) -> map (\d -> (d,sts,ts)) (departures dpts)
  let mapped = map ndList successfulNexts
  return (join mapped)

nonEmptyPair :: ([a],[b]) -> Bool
nonEmptyPair ([],[]) = False
nonEmptyPair _ = True

wrapGetThermometer :: String -> (Departure,[String],[Thermometer]) -> IO ([String],[Thermometer])
wrapGetThermometer key (d,sts,prevThermometers) = do
  tres <- getThermometer key (show $ departureCode d)
  case tres of
    Nothing -> return ([],[])
    Just tres -> return (sts,tres:prevThermometers)

getThermometerList :: String -> [(Departure,[String],[Thermometer])] -> IO [([String],[Thermometer])]
getThermometerList key departures = do
  thisFullResultThermometers <- forkMapM (wrapGetThermometer key) departures
  return (filter nonEmptyPair $ rights thisFullResultThermometers)

calculate_route :: String -> [([String],[Thermometer])] -> [String] -> Int -> IO [([String],[Thermometer])]
calculate_route key fromStopCodeList toStopCodeList maxIter = do
  dList <- getDepartureList key fromStopCodeList
  let extractedDestinations = map (\t -> case t of
                          (ds,_,_) -> ds) dList
  let departureCodes = map (show . departureCode) extractedDestinations
  putStrLn (show departureCodes)
  thermometers <- getThermometerList key dList
  putStrLn (show thermometers)
--  let rds = map reachableDestinationCodes thermometers
--  let destinationIntersections =
--        [ ([tdc],[th]) | p <- rds, (tdc,th) <- p, dc <- toStopCodeList, dc == tdc]
  return []

calculate_route_with_names :: String -> String -> String -> IO ()
calculate_route_with_names key fromStopName toStopName = do
  mFromStop <- getStops key fromStopName
  mToStop <- getStops key toStopName
  case (mFromStop,mToStop) of
    (Just fromStop,Just toStop) -> do
      let fromStopCodeList = stopCodeList fromStop
      let fromStopCodesPaired = map (\sc -> ([sc],[])) fromStopCodeList
      let toStopCodeList = stopCodeList toStop
      putStrLn (show fromStopCodeList)
      routes <- calculate_route key fromStopCodesPaired toStopCodeList 5 -- max 5 changes
      putStrLn (show routes)
      putStrLn (show toStopCodeList)
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

