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
import Data.Time.Clock
import Data.ByteString
import Data.ByteString.Lazy.Char8

data Connection = Connection {
  lineCode :: String,
  destinationName :: String,
  destinationCode :: String
  }

data Stop = Stop {
  stopCode :: String,
  stopName :: String,
  connections :: [Connection]
  }

data Stops = Stops {
  timestamp :: String,
  stops :: [Stop]
  }

parseStops :: String -> Maybe Value
parseStops = decode . Data.ByteString.Lazy.Char8.pack

