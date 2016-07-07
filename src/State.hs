module State where

  import Environment
  import CommandAST

  initTypeEnvList :: [(Key, Type)]
  initTypeEnvList = [("chat", Number), ("_", Undefined)]

  data BotState = BotState  { activeCommands  :: Env Comm,
                              updateId        :: Int,
                              token           :: String
                            }

  initBotState :: BotState
  initBotState = BotState { activeCommands  = initEnv,
                            updateId        = 0,
                            token           = ""
                          }

  data ExecState = ExecState  { typeEnv :: Env Type,
                                exprEnv :: Env Expr
                              }

  initExecState :: Int -> ExecState
  initExecState chat = ExecState  { typeEnv = envFromList initTypeEnvList,
                                    exprEnv = envFromList [("chat", Const (fromIntegral chat)), ("_", Const 0)]
                                  }
