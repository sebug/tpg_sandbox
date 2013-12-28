{-# LANGUAGE OverloadedStrings #-}
import TPG.WebAPI
import Cfg
import System.IO
import System.Directory
import Data.ByteString.Internal
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad
import TPG.Structured

main = do
  home_directory <- getHomeDirectory
  config_handle <- openFile (home_directory ++ "/.tpg_tests") ReadMode
  contents <- Data.ByteString.Lazy.hGetContents config_handle
  let key = getApiKeyFromConfigString contents
  case key of
    Nothing -> error "Did not find API key"
    Just key -> (do
                  r <- getStops key "Gardiol"
                  case r of
                    Nothing -> System.IO.putStrLn "Nothing"
                    Just o -> System.IO.putStrLn (show o))
  hClose config_handle
