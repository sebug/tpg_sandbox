{-# LANGUAGE OverloadedStrings #-}
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

data Stop = Stop {
  stopCode :: String,
  stopName :: String,
  connections :: [Connection]
  } deriving (Show)

data Stops = Stops {
  timestamp :: String,
  stops :: [Stop]
  } deriving (Show)

instance FromJSON Stops where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    return Stops { timestamp = ts, stops = [] }
  parseJSON _ = mzero

parseStops :: String -> Maybe Stops
parseStops input = decode bsInput
                   where bsInput = Data.ByteString.Lazy.Char8.pack input

