module Log where

  import Control.Exception (catch, IOException)

  import Data.Time
  import System.IO

  data LogType = Error | Warning | Info

  logInfo :: Maybe String -> LogType -> String -> IO ()
  logInfo lf lt s = do
    time <- getCurrentTime
    zone <- getCurrentTimeZone
    let now = utcToLocalTime zone time
    let logtime = formatTime defaultTimeLocale "(%F %H:%M) " now
    let loglineInit = "\ESC[34m" ++ logtime ++ "\ESC[m"
    let (logline, loglineClean) = case lt of
                                    Error   -> (loglineInit ++ errorString s, logtime ++ errorStringClean s)
                                    Warning -> (loglineInit ++ warningString s, logtime ++ warningStringClean s)
                                    Info    -> (loglineInit ++ s, logtime ++ s)
    putStrLn logline
    case lf of
      Nothing -> return ()
      Just f  -> catch (appendFile f (loglineClean ++ "\n"))
                        (\e ->  let err = show (e :: IOException)
                                in logInfo Nothing Error $ f ++ ": The file couldn't be opened.\n" ++ err)

  errorString :: String -> String
  errorString = (++) "\ESC[31m(Error)\ESC[m "

  errorStringClean :: String -> String
  errorStringClean = (++) "(Error) "

  warningString :: String -> String
  warningString = (++) "\ESC[33m(Warning)\ESC[m "

  warningStringClean :: String -> String
  warningStringClean = (++) "(Warning) "
