module Main where

  import System.IO
  import Control.Monad.IO.Class
  import System.Environment (getArgs)
  import Control.Exception (catch, IOException)
  import System.Exit
  import Data.List (partition)
  import Control.Arrow (second)

  import Map
  import CommandAST (Comm (..))
  import Parser (parseCommand, parseConfiguration, ParseResult (..))
  import Check (check)
  import Bot (mainBot, echoBot)
  import Monads (runBot, Bot)
  import State (BotState (..), initBotState)
  import Communication (managertls)
  import Log

  main :: IO ()
  main = do
    args <- getArgs
    (conf, files) <- parseArgs args
    configuration <- getConfiguration conf
    let lf = lookUp "logfile" configuration
    logInfo lf "Starting bot..."
    m    <- managertls
    case lookUp "token" configuration of
      Just tokenBot ->  logInfo lf ("Token: " ++ tokenBot) >>
                        case files of
                          [] -> do  logInfo lf "Repeating everything..."
                                    r <- runBot echoBot $ (initBotState m) { token = tokenBot }
                                    case r of
                                      Left err -> logInfo lf err -- TODO
                                      _        -> return ()
                          _  -> do  parsed <- mapM compileFile files
                                    let parsedOk  = filter ((/="") . fst) parsed
                                    let checked   = map (second check) parsedOk
                                    let (successful, failed) = partition (\(_, r) ->  case r of
                                                                                        Right _ -> True
                                                                                        _       -> False) checked
                                    mapM_ (\(n, Left err) -> logInfo lf $ n ++ ": " ++ err) failed
                                    mapM_ (\(n, _) -> logInfo lf $ n ++ ": Ok") successful
                                    let activeComms = mapFromList $ map (\(n, Right c) -> (n, c)) successful
                                    r <- runBot mainBot $ (initBotState m) {  activeCommands  = activeComms,
                                                                              token           = tokenBot,
                                                                              folder          = lookUp "newcommands" configuration,
                                                                              logFile         = lf
                                                                            }
                                    case r of
                                      Left err -> logInfo lf err -- TODO
                                      _        -> return ()
      Nothing -> logInfo lf "Missing token in configration file." >> exitFailure

  compileFile :: String -> IO (String, Comm)
  compileFile file = do
    content <- catch (readFile file) (\e -> let err = show (e :: IOException)
                                            in do
                                              logInfo Nothing $ file ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      ""  ->  return ("", Comm [] [])
      _   ->  case parseCommand content of
                Ok comm     -> return (name, comm)
                Failed err  -> do logInfo Nothing (file ++ ": " ++ err)
                                  return ("", Comm [] [])
    where name'  = takeWhile (/= '/') (reverse file)
          name   = takeWhile (/='.') (reverse name')

  parseArgs :: [String] -> IO (String, [String])
  parseArgs [] = putStrLn ("Missing arguments.\n" ++ usage) >> exitFailure
  parseArgs ("-c":(x:xs)) = return (x, xs)
  parseArgs ("-h":_) = putStrLn help >> exitSuccess
  parseArgs _  = putStrLn usage >> exitFailure

  getConfiguration :: String -> IO (Map String)
  getConfiguration conf = do
    content <- catch (readFile conf) (\e -> let err = show (e :: IOException)
                                            in do
                                              logInfo Nothing $ conf ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      "" -> logInfo Nothing "Configuration file is empty." >> exitFailure
      _  -> case parseConfiguration content of
              Ok c        -> return c
              Failed err  -> logInfo Nothing (conf ++ ": " ++ err) >> exitFailure

  usage :: String
  usage = "Usage: bot -c config [files]"

  help :: String
  help = usage
