{-# LANGUAGE FlexibleContexts #-}

module Bot where

  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class

  import TelegramAPI
  import Environment
  import CommandAST
  import Parser (ParseResult(..), parseRequest)
  import State (BotState (..), initExecState)
  import Monads (Bot, raise)
  import Execute

  echoBot :: Bot ()
  echoBot = do  s     <- get
                reply <- getUpdates (manager s) (token s) (updateId s)
                case reply of
                  Nothing   -> raise "Error Parsing!"
                  Just rep  -> case ok rep of
                                True -> case updates rep of
                                          []  -> (liftIO $ putStrLn "No Updates...") >> echoBot
                                          xs  -> do (liftIO $ putStrLn "New Updates!")
                                                    mapM_ (\x -> do liftIO $ putStrLn "Replying..."
                                                                    sendMessage (manager s) (token s) (chat_id $ chat $ message x) (maybe "(null)" id $ text $ message x)) xs
                                                    liftM Right $ put (s { updateId = (update_id (last xs) + 1) })
                                                    echoBot
                                _    -> echoBot

  mainBot :: Bot ()
  mainBot = do  s     <- get
                reply <- getUpdates (manager s) (token s) (updateId s)
                case reply of
                  Nothing   -> raise "Error Parsing!"
                  Just rep  -> case ok rep of
                                True -> case updates rep of
                                          []    -> mainBot
                                          upds  -> let  requests = map (\u ->
                                                                  ((chat_id . chat . message) u, maybe "" id $ (text . message) u)) upds
                                                        parsedRequests = map (\(c, r) -> (c, parseRequest r)) requests
                                                        requestsOk = map (\(c, Ok r) -> (c, r)) $
                                                                      filter (\(_, r) -> case r of
                                                                                          Ok _ -> True
                                                                                          _    -> False) parsedRequests
                                                    in  do  execs <- mapM (\(ch,(r,ar)) ->
                                                                                case lookUp r (activeCommands s) of
                                                                                  Just cmd -> do  exec <- liftIO $ execute ar (initExecState (manager s) ch (token s)) cmd
                                                                                                  case exec of
                                                                                                    Left err -> return (Left (r ++": "++err))
                                                                                                    _        -> return exec
                                                                                  Nothing  -> return (Left (r ++ ": not found"))) requestsOk
                                                            mapM (\exec -> case exec of
                                                                              Left err  -> liftIO $ putStrLn err
                                                                              _         -> return ()) execs
                                                            liftM Right $ put (s { updateId = (update_id (last upds) + 1) })
                                                            mainBot
                                _    -> (liftIO $ putStrLn "Failed rep!") >> mainBot
