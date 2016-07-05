module State where

  import Environment
  import CommandAST

  initCheckEnvList :: [(Key, Type)]
  initCheckEnvList = [("chat", Number), ("_", Undefined)]

  data BotState = BotState  { activeCommands  :: Env Comm,
                              updateId        :: Int,
                              token           :: String
                            }

  initBotState :: BotState
  initBotState = BotState { activeCommands  = initEnv,
                            updateId        = 0,
                            token           = ""
                          }
