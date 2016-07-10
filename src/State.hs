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

  data ExecState = ExecState  { exprEnv :: Env Expr,
                                tokenBot:: String
                              }

  initExecState :: Int -> String -> ExecState
  initExecState chat token = ExecState  { exprEnv = envFromList [("chat", Const (fromIntegral chat)), ("_", Const 0)],
                                          tokenBot = token
                                        }
