module Log where

  import Control.Exception (catch, IOException)

  import Data.Time
  import System.IO

  logInfo :: Maybe String -> String -> IO ()
  logInfo lf s = do time <- getCurrentTime
                    zone <- getCurrentTimeZone
                    let now = utcToLocalTime zone time
                    let logtime = formatTime defaultTimeLocale "(%F %H:%M) " now
                    let logline = "\ESC[34m" ++ logtime ++ "\ESC[m" ++ s
                    putStrLn logline
                    let logline = logtime ++ s
                    case lf of
                      Nothing -> return ()
                      Just f  -> catch (appendFile f (logline ++ "\n"))
                                        (\e ->  let err = show (e :: IOException)
                                                in logInfo Nothing $ f ++ ": The file couldn't be opened.\n" ++ err)
