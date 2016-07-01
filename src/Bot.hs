module Bot where

  import TelegramAPI
  import Classes
  import CommandAST
  import Parser (parseRequest)

  data BotState = BotState  { activeCommands  :: Env Comm,
                              updateId        :: Int,
                              token           :: String
                            }

  initBotState :: BotState
  initBotState = BotState { activeCommands  = initEnv,
                            updateId        = 0,
                            token           = ""
                          }

  type Bot a = IO a

  setNextUpdateId :: Int -> BotState -> BotState
  setNextUpdateId n s = s { updateId = n }

  echoBot :: BotState -> Bot ()
  echoBot s = do  reply <- getUpdates (token s) (updateId s)
                  case reply of
                    Nothing   -> putStrLn "Error Parsing" >> echoBot s
                    Just rep  -> case ok rep of
                                  True -> case updates rep of
                                            []  -> putStrLn "No Updates..." >> echoBot s
                                            xs  -> do putStrLn "New Updates!"
                                                      mapM_ (\x -> do putStrLn "Replying..."
                                                                      sendMessage (token s) (chat_id $ chat $ message x) (maybe "(null)" id $ text $ message x)) xs
                                                      echoBot (setNextUpdateId (update_id (last xs) + 1) s)
                                  _    -> echoBot s


  mainBot :: BotState -> Bot ()
  mainBot s = do  reply <- getUpdates (token s) (updateId s)
                  case reply of
                    Nothing   -> putStrLn "Error Parsing" >> mainBot s
                    Just rep  -> case ok rep of
                                  True -> case updates rep of
                                            []    -> mainBot s
                                            upds  -> do let texts = map (maybe "" id . text . message) upds
                                                        let parsedTexts = map parseRequest texts
                                                        mapM_ (putStrLn . show) parsedTexts
                                                        mainBot (setNextUpdateId (update_id (last upds) + 1) s)
                                  _    -> putStrLn "Fail!" >> mainBot s
