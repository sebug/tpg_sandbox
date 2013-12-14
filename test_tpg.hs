{-# LANGUAGE OverloadedStrings #-}
import TPG.WebAPI
import System.IO
import System.Directory
import Data.ByteString.Internal
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad
import Data.Aeson
import TPG.Structured

data Config = Config { api_key :: String }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "api_key"
  parseJSON _ = mzero

main = do
  home_directory <- getHomeDirectory
  config_handle <- openFile (home_directory ++ "/.tpg_tests") ReadMode
  contents <- Data.ByteString.Lazy.hGetContents config_handle
  let decoded = decode (contents) :: Maybe Config
  case decoded of
    Nothing -> error "Did not find API key"
    Just Config { api_key = key } -> (do
                  results <- getStops key "Gardiol"
                  case (parseStops results) of
                    Nothing -> System.IO.putStrLn "Nothing"
                    Just o -> System.IO.putStrLn (show o))
  hClose config_handle
