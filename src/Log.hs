module Log where

  import Data.Time
  import System.IO

  logIt :: String -> IO () --String
  logIt s = do  time <- getCurrentTime
                zone <- getCurrentTimeZone
                let now = utcToLocalTime zone time
                let logtime = formatTime defaultTimeLocale "(%F %H:%M) " now
                let logline = logtime ++ s
                putStrLn logline
                --return logline
