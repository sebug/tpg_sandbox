{-# LANGUAGE OverloadedStrings #-}
import System.Directory
import System.Environment
import System.IO
import qualified Data.ByteString.Lazy as BS
import Cfg

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
        Just key -> (do
                        putStrLn key)
      hClose config_handle

