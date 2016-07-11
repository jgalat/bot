module State where

  import Network.HTTP.Conduit (Manager)

  import Environment
  import CommandAST

  initTypeEnvList :: [(Key, Type)]
  initTypeEnvList = [("chat", Number), ("_", Undefined)]

  data BotState = BotState  { activeCommands  :: Env Comm,
                              updateId        :: Int,
                              token           :: String,
                              manager         :: Manager
                            }

  initBotState :: Manager -> BotState
  initBotState m = BotState { activeCommands  = initEnv,
                              updateId        = 0,
                              token           = "",
                              manager         = m
                            }

  data ExecState = ExecState  { exprEnv     :: Env Expr,
                                tokenBot    :: String,
                                managerBot  :: Manager
                              }

  initExecState :: Manager -> Int -> String -> ExecState
  initExecState m chat token = ExecState  { exprEnv = envFromList [("chat", Const (fromIntegral chat)), ("_", Const 0)],
                                            tokenBot = token,
                                            managerBot = m
                                          }
