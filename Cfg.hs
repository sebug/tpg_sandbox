{-# LANGUAGE OverloadedStrings #-}
module Cfg
(
  Config(Config)
, getApiKeyFromConfigString
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy

data Config = Config { api_key :: String }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "api_key"
  parseJSON _ = mzero

getApiKeyFromConfigString :: ByteString -> Maybe String
getApiKeyFromConfigString = getApiKey . decode

getApiKey :: Maybe Config -> Maybe String
getApiKey Nothing = Nothing
getApiKey (Just (Config { api_key = k })) = Just k

