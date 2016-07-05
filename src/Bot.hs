{-# LANGUAGE FlexibleContexts #-}

module Bot where

  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class

  import TelegramAPI
  import Environment
  import CommandAST
  import Parser (parseRequest)
  import State
  import Monads (Bot, raise)

  echoBot :: Bot (Either String ())
  echoBot = do  s     <- get
                reply <- getUpdates (token s) (updateId s)
                case reply of
                  Nothing   -> raise "Error Parsing!"
                  Just rep  -> case ok rep of
                                True -> case updates rep of
                                          []  -> (liftIO $ putStrLn "No Updates...") >> echoBot
                                          xs  -> do (liftIO $ putStrLn "New Updates!")
                                                    mapM_ (\x -> do liftIO $ putStrLn "Replying..."
                                                                    sendMessage (token s) (chat_id $ chat $ message x) (maybe "(null)" id $ text $ message x)) xs
                                                    liftM Right $ put (s { updateId = (update_id (last xs) + 1) })
                                                    echoBot
                                _    -> echoBot

  mainBot :: Bot (Either String ())
  mainBot = do  s     <- get
                reply <- getUpdates (token s) (updateId s)
                case reply of
                  Nothing   -> raise "Error Parsing!"
                  Just rep  -> case ok rep of
                                True -> case updates rep of
                                          []    -> mainBot
                                          upds  -> let  texts = map (maybe "" id . text . message) upds
                                                        parsedTexts = map parseRequest texts
                                                    in  do  mapM_ (liftIO . putStrLn . show) parsedTexts
                                                            liftM Right $ put (s { updateId = (update_id (last upds) + 1) })
                                                            mainBot
                                _    -> (liftIO $ putStrLn "Fail!") >> mainBot
