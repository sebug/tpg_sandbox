{-# LANGUAGE OverloadedStrings #-}
--
-- Got some ideas from here: http://www.the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
module TPG.Structured
( Connection
, Stop
, Stops
, parseStops
) where

import TPG.WebAPI
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.ByteString
import Data.ByteString.Lazy.Char8

data Connection = Connection {
  lineCode :: String,
  destinationName :: String,
  destinationCode :: String
  } deriving (Show)

instance FromJSON Connection where
  parseJSON (Object v) = Connection <$>
                         v .: "lineCode" <*>
                         v .: "destinationName" <*>
                         v .: "destinationCode"
  parseJSON _ = mzero

data Stop = Stop {
  stopCode :: String,
  stopName :: String,
  connections :: [Connection]
  } deriving (Show)

instance FromJSON Stop where
  parseJSON (Object v) = do
    stCode <- v .: "stopCode"
    stName <- v .: "stopName"
    cnx <- parseJSON =<< (v .: "connections")
    return Stop { stopCode = stCode, stopName = stName, connections = cnx }
  parseJSON _ = mzero
    

data Stops = Stops {
  timestamp :: String,
  stops :: [Stop]
  } deriving (Show)

instance FromJSON Stops where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    sts <- parseJSON =<< (v .: "stops")
    return Stops { timestamp = ts, stops = sts }
  parseJSON _ = mzero

parseStops :: String -> Maybe Stops
parseStops input = decode bsInput
                   where bsInput = Data.ByteString.Lazy.Char8.pack input

