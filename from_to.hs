{-# LANGUAGE OverloadedStrings #-}
import TPG.WebAPI
import TPG.Structured
import System.Directory
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BS
import Cfg

getDepartureList :: String -> [String] -> IO [Departure]
getDepartureList key [] = return []
getDepartureList key (stopCode:scs) = do
  thisNextDepartures <- getNextDepartures key stopCode
  let ndList = case thisNextDepartures of
        Nothing -> []
        Just dpts -> (departureList dpts)
  otherNextDepartures <- getDepartureList key scs
  return (ndList ++ otherNextDepartures)

calculate_route :: String -> String -> String -> IO ()
calculate_route key fromStopName toStopName = do
  mFromStop <- getStops key fromStopName
  mToStop <- getStops key toStopName
  case (mFromStop,mToStop) of
    (Just fromStop, Just toStop) -> do
      putStrLn (show (stopCodeList fromStop))
      dList <- getDepartureList key (stopCodeList fromStop)
      putStrLn (show dList)
      putStrLn (show (stopCodeList toStop))
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
        Just key -> calculate_route key (head args) (head (tail args))
      hClose config_handle

