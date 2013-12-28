{-# LANGUAGE OverloadedStrings #-}
import TPG.WebAPI
import Cfg
import System.IO
import System.Directory
import System.Environment
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as BS
import Control.Applicative
import Control.Monad
import TPG.Structured

dispatch :: [(String, (String,[String]) -> IO())]
dispatch = [("getStops",
             oneArg (showResults1 . getStops) "Usage: getStops stopName"
            ),
            ("getNextDepartures",
             oneArg (showResults1 . getNextDepartures) "Usage: getNextDepartures stopCode"),
            ("getAllNextDepartures",
             threeArgs (showResults3 . getAllNextDepartures) "Usage: getAllNextDepartures stopCode lineCode destinationCode"),
            ("getThermometer",
             oneArg (showResults1 . getThermometer) "Usage: getThermometer departureCode"),
            ("getThermometerPhysicalStops",
             oneArg (showResults1 . getThermometerPhysicalStops) "Usage: getThermometerPhysicalStops departureCode"),
            ("getLinesColors",
             noArg (bindShowResult . getLinesColors) "Usage: getLinesColors"),
            ("getDisruptions",
             noArg (bindShowResult . getDisruptions) "Usage: getDisruptions")]

showResults1 :: Show a => (String -> IO (Maybe a)) -> (String -> IO())
showResults1 = (.) bindShowResult

showResults2 :: Show a => (String -> String -> IO (Maybe a)) -> (String -> String -> IO())
showResults2 f x = bindShowResult . (f x)

showResults3 :: Show a => (String -> String -> String -> IO (Maybe a)) -> (String -> String -> String -> IO())
showResults3 f x y = bindShowResult . (f x y)

bindShowResult :: Show a => IO (Maybe a) -> IO ()
bindShowResult = (=<<) (putStrLn . maybeShow)

noArg :: (String -> IO()) -> String -> (String,[String]) -> IO()
noArg f _ (key,[]) = f key
noArg _ usage _ = putStrLn usage

oneArg :: (String -> String -> IO()) -> String -> (String,[String]) -> IO()
oneArg f _ (key,[arg1]) = f key arg1
oneArg _ usage _ = putStrLn usage

twoArgs :: (String -> String -> String -> IO()) -> String -> (String,[String]) -> IO()
twoArgs f _ (key,[arg1,arg2]) = f key arg1 arg2
twoArgs _ usage _ = putStrLn usage

threeArgs :: (String -> String -> String -> String -> IO()) -> String -> (String,[String]) -> IO()
threeArgs f _ (key,[arg1,arg2,arg3]) = f key arg1 arg2 arg3
threeArgs _ usage _ = putStrLn usage

maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = "No result."
maybeShow (Just o) = show o

main = do
  (command:args) <- getArgs
  home_directory <- getHomeDirectory
  config_handle <- openFile (home_directory ++ "/.tpg_tests") ReadMode
  contents <- BS.hGetContents config_handle
  let key = getApiKeyFromConfigString contents
  case key of
    Nothing -> error "Did not find API key"
    Just key -> (do
                    let mact = lookup command dispatch
                    case mact of
                      Nothing -> putStrLn $ "Did not find command: " ++ command
                      Just action -> action (key,args))
  hClose config_handle
