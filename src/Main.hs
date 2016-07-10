module Main where

  import System.IO
  import Control.Monad.IO.Class
  import System.Environment (getArgs)
  import Control.Exception (catch, IOException)
  import Data.List

  import Environment
  import CommandAST (Comm (..))
  import Parser (parseCommand, ParseResult (..))
  import Check (check)
  import Bot (mainBot, echoBot)
  import Monads (runBot, Bot)
  import State (BotState (..), initBotState)
  import Keys

  main :: IO ()
  main = do putStrLn "Saluton Mondo"
            args <- getArgs
            case args of
              []      -> do putStrLn "Repeating everything..."
                            r <- runBot echoBot $ initBotState { token = tokenBot }
                            case r of
                              Left err -> putStrLn "Error" -- TODO
                              _        -> return ()
              files   -> do parsed <- mapM compileFile files
                            let parsedOk  = filter ((/="") . fst) parsed
                            let checked   = map (\(n, c) -> (n, check c)) parsedOk
                            let (successful, failed) = partition (\(_, r) ->  case r of
                                                                                Right _ -> True
                                                                                _       -> False) checked
                            mapM_ (\(n, Left err) -> putStrLn $ n ++ ": " ++ err) failed
                            mapM_ (\(n, _) -> putStrLn $ n ++ ": Ok") successful
                            let activeComms = envFromList $ map (\(n, Right c) -> (n, c)) successful
                            r <- runBot mainBot $ initBotState {  activeCommands  = activeComms,
                                                                  token           = tokenBot }
                            case r of
                              Left err -> putStrLn "Error" -- TODO
                              _        -> return ()


  compileFile :: MonadIO m => String -> m (String, Comm)
  compileFile file = do content <- liftIO $ catch (readFile file) (\e ->  let err = show (e :: IOException)
                                                                          in do putStrLn $ file ++ ": No se pudo abrir el archivo. " ++ err
                                                                                return "")
                        case content of
                          ""  ->  return ("", Comm [] [])
                          _   ->  case parseCommand content of
                                    Ok comm     -> return (name, comm)
                                    Failed str  -> do liftIO $ putStrLn (file ++ ": " ++ str)
                                                      return ("", Comm [] [])
                     where
                       name'  = fst $ span (/='/') $ reverse file
                       name   = fst $ span (/='.') $ reverse name'
