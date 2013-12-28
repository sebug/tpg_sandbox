module TPG.WebAPI.Bases
 ( apiCall
 , withArg
 , andArg
 , andArg2
 ) where

baseAddress :: String
baseAddress = "http://rtpi.data.tpg.ch/v1/"

apiCall :: String -> String -> String
apiCall apiKey method = baseAddress ++ method ++ "?key=" ++ apiKey

withArg :: String -> String -> String -> String
ac `withArg` argName = \ argValue -> ac ++ "&" ++ argName ++ "=" ++ argValue

andArg :: (String -> String) -> String -> (String -> String -> String)
ac `andArg` argName = \ argValue1 argValue2 -> (ac argValue1) ++ "&" ++ argName ++ "=" ++ argValue2

andArg2 :: (String -> String -> String) -> String -> (String -> String -> String -> String)
ac `andArg2` argName = \ argValue1 argValue2 argValue3 -> (ac argValue1 argValue2) ++ "&" ++ argName ++ "=" ++ argValue3
