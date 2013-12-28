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
             oneArg (showResults1 . getStops) "getStops stopName"
            ),
            ("getNextDepartures",
             oneArg (showResults1 . getNextDepartures) "getNextDepartures stopCode"),
            ("getAllNextDepartures",
             threeArgs (showResults3 . getAllNextDepartures) "getAllNextDepartures stopCode lineCode destinationCode"),
            ("getThermometer",
             oneArg (showResults1 . getThermometer) "getThermometer departureCode"),
            ("getThermometerPhysicalStops",
             oneArg (showResults1 . getThermometerPhysicalStops) "getThermometerPhysicalStops departureCode"),
            ("getLinesColors",
             noArg (bindShowResult . getLinesColors) "getLinesColors"),
            ("getDisruptions",
             noArg (bindShowResult . getDisruptions) "getDisruptions")]

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
oneArg _ usage _ = putStrLn ("Usage: " ++ usage)

twoArgs :: (String -> String -> String -> IO()) -> String -> (String,[String]) -> IO()
twoArgs f _ (key,[arg1,arg2]) = f key arg1 arg2
twoArgs _ usage _ = putStrLn ("Usage: " ++ usage)

threeArgs :: (String -> String -> String -> String -> IO()) -> String -> (String,[String]) -> IO()
threeArgs f _ (key,[arg1,arg2,arg3]) = f key arg1 arg2 arg3
threeArgs _ usage _ = putStrLn ("Usage: " ++ usage)

maybeShow :: Show a => Maybe a -> String
maybeShow Nothing = "No result."
maybeShow (Just o) = show o

main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Please provide one of the following commands as parameter:"
      putStrLn $ foldl (++) "" $ map (\p -> (fst p) ++ "\n") dispatch
    (command:rest) -> do
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
                          Just action -> action (key,rest))
      hClose config_handle
