module Main where

  import System.IO
  import System.Environment
  import Control.Exception (catch, IOException)

  import TelegramAPI
  import Keys
  import CommandAST
  import Parser


  data State = State {  activeCommands  :: [Command],
                        nextUpdateId    :: Int
                      }

  initState :: State
  initState = State [] 0

  setNextUpdateId :: Int -> State -> State
  setNextUpdateId n st = st { nextUpdateId = n }

  main :: IO ()
  main = do putStrLn "Hello World"
            args <- getArgs
            case args of
              []      -> do putStrLn "Repeating everything..."
                            echoBot initState
              comms   -> do compiled <- mapM compileFile comms
                            let activeComms = filter (\x -> case x of
                                                              FailedCommand -> False
                                                              _             -> True) compiled
                            mainBot $ initState { activeCommands = activeComms }

  compileFile :: String -> IO Command
  compileFile file = do content <- catch (readFile file) (\e -> let err = show (e :: IOException)
                                                                in do putStrLn $ "No se pudo abrir el archivo " ++ file ++ " :" ++ err
                                                                      return "")
                        case parseCommand content of
                          Ok comm     -> return $ Command name comm
                          Failed str  -> do putStrLn $ file ++ ": " ++ str
                                            return FailedCommand
                     where
                       name'  = fst $ span (/='/') $ reverse file
                       name   = fst $ span (/='.') $ reverse name'

  mainBot :: State -> IO ()
  mainBot st = do mapM (putStrLn . show) $ activeCommands st
                  return ()

  echoBot :: State -> IO ()
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
