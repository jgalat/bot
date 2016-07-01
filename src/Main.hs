module Main where

  import System.IO
  import Control.Monad.IO.Class
  import System.Environment
  import Control.Exception (catch, IOException)
  import qualified Data.Map as M


  import CommandAST
  import Parser
  import Check
  import Bot
  import Keys

  main :: IO ()
  main = do putStrLn "Saluton Mondo"
            args <- getArgs
            case args of
              []      -> do putStrLn "Repeating everything..."
                            echoBot $ initBotState { token = tokenBot }
              files   -> do parsed <- mapM compileFile files
                            let parsedOk  = filter ((/="") . fst) parsed
                            let checked   = map (\(n, c) -> (n, check c)) parsedOk
                            let failed    = filter (\(_, r) ->  case r of
                                                                  Left _ -> True
                                                                  _      -> False) checked
                            mapM_ (\(n, Left err) -> putStrLn $ n ++ ": " ++ err) failed
                            let checkedOk = M.difference (M.fromList parsedOk) (M.fromList failed)
                            mapM_ (\(n, _) -> putStrLn $ n ++ ": Ok") (M.toList checkedOk)
                            mainBot $ initBotState {  activeCommands = checkedOk,
                                                      token = tokenBot }

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
