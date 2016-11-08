module Main where

  import System.IO
  import Control.Monad
  import Control.Monad.IO.Class
  import System.Environment (getArgs)
  import Control.Exception (catch, IOException)
  import System.Exit
  import Data.List (partition)
  import Control.Arrow (second)
  import System.Console.GetOpt

  import Map
  import CommandAST (Comm (..))
  import Parser (parseCommand, parseConfiguration, ParseResult (..))
  import Check (check)
  import Bot (mainBot, debugBot)
  import Monads (runBot, Bot)
  import State (BotState (..), initBotState, initDebugBotState)
  import Communication (managertls)
  import Log
  import PrettyPrint

  startMainBot :: String -> [String] -> IO ()
  startMainBot conf files = do
    configuration <- getConfiguration conf
    let lf = getConfigSetting "logfile" configuration
    let nc = getConfigSetting "newcommands" configuration
    logInfo lf Info "Starting bot..."
    m    <- managertls
    case lookUp "token" configuration of
      Just tokenBot ->  do  logInfo lf Info ("Token: " ++ tokenBot)
                            activeComms <- getCommands lf files
                            r <- runBot mainBot $ (initBotState m) {  activeCommands  = activeComms,
                                                                      token           = tokenBot,
                                                                      folder          = nc,
                                                                      logFile         = lf
                                                                    }
                            case r of
                              Left err -> logInfo lf Error err
                              _        -> return ()
      Nothing -> logInfo lf Error "Missing token in configration file." >> exitFailure

  startDebugBot :: [String] -> IO ()
  startDebugBot files = do
    logInfo Nothing Info "Starting debugbot..."
    activeComms <- getCommands Nothing files
    m <- managertls
    r <- runBot debugBot $ (initDebugBotState m) { activeCommands = activeComms }
    case r of
      Left err  -> logInfo Nothing Error err
      _         -> return ()

  getCommands :: Maybe String -> [String] -> IO (Map Comm)
  getCommands lf files = do
    parsed <- mapM compileFile files
    let parsedOk  = filter ((/="") . fst) parsed
    let checked   = map (second check) parsedOk
    let (successful, failed) = partition (\(_, r) ->  case r of
                                                        Right _ -> True
                                                        _       -> False) checked
    mapM_ (\(n, Left err) -> logInfo lf Error $ n ++ ": " ++ err) failed
    mapM_ (\(n, _) -> logInfo lf Info $ n ++ ": Ok") successful
    let activeComms = mapFromList $ map (\(n, Right c) -> (n, c)) successful
    return activeComms

  compileFile :: String -> IO (String, Comm)
  compileFile file = do
    content <- catch (readFile file) (\e -> let err = show (e :: IOException)
                                            in do
                                              logInfo Nothing Error $ file ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      ""  ->  return ("", Comm [] [])
      _   ->  case parseCommand content of
                Ok comm     -> return (name, comm)
                Failed err  -> do logInfo Nothing Error (file ++ ": " ++ err)
                                  return ("", Comm [] [])
    where name'  = takeWhile (/= '/') (reverse file)
          name   = takeWhile (/='.') (reverse name')

  data Flag = Help | Version | Debug | Configuration String
            deriving Eq

  options :: [OptDescr Flag]
  options = [ Option ['h'] ["help"]           (NoArg Help)                  "Print this help message"
            , Option ['v'] ["version"]        (NoArg Version)               "Print version"
            , Option ['o'] ["offline"]        (NoArg Debug)                 "Start offline mode"
            , Option ['c'] ["configuration"]  (ReqArg Configuration "FILE") "Set configuration file"
            ]

  parseArgs :: [String] -> IO ([Flag], [String])
  parseArgs argv = case getOpt Permute options argv of
                      (f, fs, []) -> return (f, fs)
                      (_, _, err) -> ioError (userError (concat err ++ usageInfo usage options))

  getConfiguration :: String -> IO (Map String)
  getConfiguration conf = do
    content <- catch (readFile conf) (\e -> let err = show (e :: IOException)
                                            in do
                                              logInfo Nothing Error $ conf ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      "" -> logInfo Nothing Error "Configuration file is empty." >> exitFailure
      _  -> case parseConfiguration content of
              Ok c        -> return c
              Failed err  -> logInfo Nothing Error (conf ++ ": " ++ err) >> exitFailure

  getConfigSetting :: String -> Map String -> Maybe String
  getConfigSetting p conf = case lookUp p conf of
                              Just "" -> Nothing
                              x       -> x

  usage :: String
  usage = "Usage: bot OPTION [FILES]"

  help :: String
  help = usage

  version :: String
  version = "Bot 0.0.1"

  main :: IO ()
  main = do
    args <- getArgs
    (flags, files) <- parseArgs args
    when (elem Help flags)    (putStrLn (usageInfo help options) >> exitSuccess)
    when (elem Version flags) (putStrLn version >> exitSuccess)
    let mode = filter modeFlags flags
    case mode of
      [Configuration conf]  -> startMainBot conf files
      [Debug]               -> startDebugBot files
      _                     -> putStrLn (usageInfo usage options) >>
                               exitFailure
    where modeFlags Debug = True
          modeFlags (Configuration _) = True
          modeFlags _ = False
