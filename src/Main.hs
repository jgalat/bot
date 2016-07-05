module Main where

  import System.IO
  import Control.Monad.IO.Class
  import System.Environment
  import Control.Exception (catch, IOException)

  import Environment
  import CommandAST
  import Parser
  import Check
  import Bot
  import Monads
  import State
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
                            let failed    = filter (\(_, r) ->  case r of
                                                                  Left _ -> True
                                                                  _      -> False) checked
                            mapM_ (\(n, Left err) -> putStrLn $ n ++ ": " ++ err) failed
                            let checkedOk = envDifference (envFromList parsedOk) (envFromList failed)
                            mapM_ (\(n, _) -> putStrLn $ n ++ ": Ok") (envToList checkedOk)
                            r <- runBot mainBot $ initBotState {  activeCommands = checkedOk,
                                                                  token = tokenBot }
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
