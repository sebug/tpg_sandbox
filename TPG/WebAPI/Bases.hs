module TPG.WebAPI.Bases
 ( apiCall
 , withArg
 ) where

baseAddress :: String
baseAddress = "http://rtpi.data.tpg.ch/v1/"

apiCall :: String -> String -> String
apiCall apiKey method = baseAddress ++ method ++ "?key=" ++ apiKey

withArg :: String -> String -> String -> String
ac `withArg` argName = \ argValue -> ac ++ "&" ++ argName ++ "=" ++ argValue
