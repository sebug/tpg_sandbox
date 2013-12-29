{-# LANGUAGE OverloadedStrings #-}
--
-- Got some ideas from here: http://www.the-singleton.com/2012/02/parsing-nested-json-in-haskell-with-aeson/
module TPG.Structured
( Connection
, Stop
, Stops
, NextDepartures
, Thermometer
, ThermometerPhysicalStops
, LineColors
, Disruptions
, parseStops
, parseNextDepartures
, parseThermometer
, parseThermometerPhysicalStops
, parseLineColors
, parseDisruptions
, stopCodeList
) where

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
parseStops = decode . Data.ByteString.Lazy.Char8.pack

stopCodeList :: Stops -> [String]
stopCodeList sts = Prelude.map stopCode $ stops sts

data Departure = Departure {
  departureCode :: Int,
  timestampDeparture :: String,
  waitingTimeMillis :: Int,
  waitingTime :: String,
  line :: Connection,
  reliability :: String
  } deriving (Show)

instance FromJSON Departure where
  parseJSON (Object v) = do
    dc <- v .: "departureCode"
    ts <- v .: "timestamp"
    wtM <- v .: "waitingTimeMillis"
    wt <- v .: "waitingTime"
    l <- parseJSON =<< (v .: "line")
    r <- v .: "reliability"
    return Departure {
      departureCode = dc,
      timestampDeparture = ts,
      waitingTimeMillis = wtM,
      waitingTime = wt,
      line = l,
      reliability = r
      }
  parseJSON _ = mzero

data NextDepartures = NextDepartures {
  timestampNextDepartures :: String,
  stop :: Stop,
  departures :: [Departure]
  } deriving (Show)

instance FromJSON NextDepartures where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    st <- v .: "stop"
    dpts <- parseJSON =<< (v .: "departures")
    return NextDepartures {
      timestampNextDepartures = ts,
      stop = st,
      departures = dpts
      }
  parseJSON _ = mzero

parseNextDepartures :: String -> Maybe NextDepartures
parseNextDepartures = decode . Data.ByteString.Lazy.Char8.pack

data Step = Step {
  departureCodeStep :: Int,
  timestampStep :: String,
  stopStep :: Stop,
  deviation :: Bool,
  reliabilityStep :: String,
  visible :: Bool
  } deriving (Show)

instance FromJSON Step where
  parseJSON (Object v) = do
    dc <- v .: "departureCode"
    ts <- v .: "timestamp"
    st <- v .: "stop"
    dev <- v .: "deviation"
    rs <- v .: "reliability"
    vs <- v .: "visible"
    return Step {
      departureCodeStep = dc,
      timestampStep = ts,
      stopStep = st,
      deviation = dev,
      reliabilityStep = rs,
      visible = vs
      }
  parseJSON _ = mzero

data Thermometer = Thermometer {
  timestampThermometer :: String,
  stopThermometer :: Stop,
  lineCodeThermometer :: String,
  destinationNameThermometer :: String,
  destinationCodeThermometer :: String,
  steps :: [Step]
} deriving (Show)

instance FromJSON Thermometer where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    st <- v .: "stop"
    lc <- v .: "lineCode"
    dn <- v .: "destinationName"
    dc <- v .: "destinationCode"
    sts <- parseJSON =<< (v .: "steps")
    return Thermometer {
      timestampThermometer = ts,
      stopThermometer = st,
      lineCodeThermometer = lc,
      destinationNameThermometer = dn,
      destinationCodeThermometer = dc,
      steps = sts
      }
  parseJSON _ = mzero

parseThermometer :: String -> Maybe Thermometer
parseThermometer = decode . Data.ByteString.Lazy.Char8.pack

data Coordinates = Coordinates {
  longitude :: Float,
  latitude :: Float,
  referential :: String
  } deriving (Show)

instance FromJSON Coordinates where
  parseJSON (Object v) = do
    lat <- v .: "latitude"
    long <- v .: "longitude"
    r <- v .: "referential"
    return Coordinates {
      latitude = lat,
      longitude = long,
      referential = r
      }
  parseJSON _ = mzero

data PhysicalStop = PhysicalStop {
  physicalStopCode :: String,
  stopNamePhysicalStop :: String,
  connectionsPhysicalStop :: [Connection],
  coordinates :: Coordinates
  } deriving (Show)

instance FromJSON PhysicalStop where
  parseJSON (Object v) = do
    stCode <- v .: "physicalStopCode"
    stName <- v .: "stopName"
    cnx <- parseJSON =<< (v .: "connections")
    coords <- v .: "coordinates"
    return PhysicalStop { physicalStopCode = stCode, stopNamePhysicalStop = stName, connectionsPhysicalStop = cnx, coordinates = coords }
  parseJSON _ = mzero

data PhysicalStep = PhysicalStep {
  departureCodePS :: Int,
  timestampPS :: String,
  stopPS :: Stop,
  physicalStopPS :: PhysicalStop,
  deviationPS :: Bool,
  reliabilityPS :: String,
  visiblePS :: Bool
  } deriving (Show)

instance FromJSON PhysicalStep where
  parseJSON (Object v) = do
    dc <- v .: "departureCode"
    ts <- v .: "timestamp"
    st <- v .: "stop"
    pst <- v .: "physicalStop"
    dev <- v .: "deviation"
    rs <- v .: "reliability"
    vs <- v .: "visible"
    return PhysicalStep {
      departureCodePS = dc,
      timestampPS = ts,
      stopPS = st,
      physicalStopPS = pst,
      deviationPS = dev,
      reliabilityPS = rs,
      visiblePS = vs
      }
  parseJSON _ = mzero

data ThermometerPhysicalStops = ThermometerPhysicalStops {
  timestampThermometerPhysicalStops :: String,
  stopThermometerPhysicalStops :: Stop,
  lineCodeThermometerPhysicalStops :: String,
  destinationNameThermometerPhysicalStops :: String,
  destinationCodeThermometerPhysicalStops :: String,
  stepsThermometerPhysicalStops :: [PhysicalStep]
  } deriving (Show)

instance FromJSON ThermometerPhysicalStops where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    st <- v .: "stop"
    lc <- v .: "lineCode"
    dn <- v .: "destinationName"
    dc <- v .: "destinationCode"
    sts <- parseJSON =<< (v .: "steps")
    return ThermometerPhysicalStops {
      timestampThermometerPhysicalStops = ts,
      stopThermometerPhysicalStops = st,
      lineCodeThermometerPhysicalStops = lc,
      destinationNameThermometerPhysicalStops = dn,
      destinationCodeThermometerPhysicalStops = dc,
      stepsThermometerPhysicalStops = sts
      }
  parseJSON _ = mzero

parseThermometerPhysicalStops :: String -> Maybe ThermometerPhysicalStops
parseThermometerPhysicalStops = decode . Data.ByteString.Lazy.Char8.pack

data Color = Color {
  lineCodeColor :: String,
  hexa :: String
  } deriving (Show)

instance FromJSON Color where
  parseJSON (Object v) = do
    lc <- v .: "lineCode"
    c <- v .: "hexa"
    return Color {
      lineCodeColor = lc,
      hexa = c
      }
  parseJSON _ = mzero

data LineColors = LineColors {
  timestampLineColors :: String,
  colors :: [Color]
  } deriving (Show)

instance FromJSON LineColors where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    cs <- parseJSON =<< (v .: "colors")
    return LineColors {
      timestampLineColors = ts,
      colors = cs
      }
  parseJSON _ = mzero


parseLineColors :: String -> Maybe LineColors
parseLineColors = decode . Data.ByteString.Lazy.Char8.pack

data Disruption = Disruption {
  disruptionCode :: Int,
  timestampDisruption :: String,
  place :: String,
  nature :: String,
  consequence :: String,
  lineCodeDisruption :: String,
  stopNameDisruption :: String
  } deriving (Show)

instance FromJSON Disruption where
  parseJSON (Object v) = do
    dc <- v .: "disruptionCode"
    ts <- v .: "timestamp"
    p  <- v .: "place"
    n  <- v .: "nature"
    c  <- v .: "consequence"
    lc <- v .: "lineCode"
    sn <- v .: "stopName"
    return Disruption {
      disruptionCode = dc,
      timestampDisruption = ts,
      place = p,
      nature = n,
      consequence = c,
      lineCodeDisruption = lc,
      stopNameDisruption = sn
      }
  parseJSON _ = mzero

data Disruptions = Disruptions {
  timestampDisruptions :: String,
  disruptions :: [Disruption]
  } deriving (Show)

instance FromJSON Disruptions where
  parseJSON (Object v) = do
    ts <- v .: "timestamp"
    ds <- parseJSON =<< (v .: "disruptions")
    return Disruptions {
      timestampDisruptions = ts,
      disruptions = ds
      }
  parseJSON _ = mzero


parseDisruptions :: String -> Maybe Disruptions
parseDisruptions = decode . Data.ByteString.Lazy.Char8.pack
