module Main where

  import System.IO
  import Control.Monad.IO.Class
  import System.Environment
  import Control.Exception (catch, IOException)
  import Data.Map.Lazy (fromList)

  import TelegramAPI
  import Keys
  import CommandAST
  import Parser
  import Classes

  data MainEnv = MainEnv  { activeCommands  :: Env Comm,
                            nextUpdateId    :: Int
                          }

  initMainEnv :: MainEnv
  initMainEnv = MainEnv { activeCommands = initEnv,
                          nextUpdateId   = 0
                        }

  setNextUpdateId :: Int -> MainEnv -> MainEnv
  setNextUpdateId n st = st { nextUpdateId = n }

  main :: IO ()
  main = do putStrLn "Hello World"
            args <- getArgs
            case args of
              []      -> do putStrLn "Repeating everything..."
                            echoBot initMainEnv
              comms   -> do compiled <- mapM compileFile comms
                            let activeComms = fromList $ filter ((/="") . fst) compiled
                            mainBot $ initMainEnv { activeCommands = activeComms }

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

  mainBot :: MainEnv -> IO ()
  mainBot st = do mapM (putStrLn . show) $ activeCommands st
                  return ()

  echoBot :: MainEnv -> IO ()
  echoBot st = do reply <- getUpdates tokenBot (nextUpdateId st)
                  case reply of
                    Nothing   -> putStrLn "Error Parsing"
                    Just rep  -> case ok rep of
                                    True -> case updates rep of
                                              []  -> do putStrLn "No Updates..."
                                                        echoBot st
                                              xs  -> do putStrLn "New Updates!"
                                                        mapM (\x -> do  putStrLn "Replying..."
                                                                        sendMessage tokenBot (chat_id $ chat $ message x) (maybe "(null)" id $ text $ message x)) xs
                                                        echoBot $ setNextUpdateId (update_id (last xs) + 1) st
