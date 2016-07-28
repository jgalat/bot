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

  main :: IO ()
  main = do
    putStrLn "Saluton Mondo"
    args <- getArgs
    (conf, files) <- parseArgs args
    configuration <- getConfiguration conf
    m    <- managertls
    case lookUp "token" configuration of
      Just tokenBot ->  putStrLn ("Token: " ++ tokenBot) >>
                        case files of
                          [] -> do  putStrLn "Repeating everything..."
                                    r <- runBot echoBot $ (initBotState m) { token = tokenBot }
                                    case r of
                                      Left err -> putStrLn err -- TODO
                                      _        -> return ()
                          _  -> do  parsed <- mapM compileFile files
                                    let parsedOk  = filter ((/="") . fst) parsed
                                    let checked   = map (second check) parsedOk
                                    let (successful, failed) = partition (\(_, r) ->  case r of
                                                                                        Right _ -> True
                                                                                        _       -> False) checked
                                    mapM_ (\(n, Left err) -> putStrLn $ n ++ ": " ++ err) failed
                                    mapM_ (\(n, _) -> putStrLn $ n ++ ": Ok") successful
                                    let activeComms = mapFromList $ map (\(n, Right c) -> (n, c)) successful
                                    r <- runBot mainBot $ (initBotState m) {  activeCommands  = activeComms,
                                                                              token           = tokenBot,
                                                                              folder          = lookUp "newcommands" configuration
                                                                            }
                                    case r of
                                      Left err -> putStrLn err -- TODO
                                      _        -> return ()
      Nothing -> putStrLn "Missing token in configration file." >> exitFailure

  compileFile :: String -> IO (String, Comm)
  compileFile file = do
    content <- catch (readFile file) (\e -> let err = show (e :: IOException)
                                            in do
                                              putStrLn $ file ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      ""  ->  return ("", Comm [] [])
      _   ->  case parseCommand content of
                Ok comm     -> return (name, comm)
                Failed err  -> do putStrLn (file ++ ": " ++ err)
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
                                              putStrLn $ conf ++ ": The file couldn't be opened.\n" ++ err
                                              return "")
    case content of
      "" -> putStrLn "Configuration file is empty." >> exitFailure
      _  -> case parseConfiguration content of
              Ok c        -> return c
              Failed err  -> putStrLn (conf ++ ": " ++ err) >> exitFailure

  usage :: String
  usage = "Usage: bot -c config [files]"

  help :: String
  help = usage
