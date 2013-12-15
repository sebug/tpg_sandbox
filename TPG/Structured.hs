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

parseStops :: String -> Maybe Stops
parseStops input = do result <- decode bsInput
                      flip parseMaybe result $ \obj -> do
                        ts <- obj .: "timestamp"
                        return Stops { timestamp=ts, stops=[] }
                   where bsInput = Data.ByteString.Lazy.Char8.pack input

