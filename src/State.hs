module State where

  import Network.HTTP.Conduit (Manager)

  import Map
  import CommandAST

  initCheckMapList :: [(Key, ())]
  initCheckMapList = [("chat", ()), ("_", ())]

  data BotState = BotState  { activeCommands  :: Map Comm,
                              updateId        :: Int,
                              token           :: String,
                              manager         :: Manager
                            }

  initBotState :: Manager -> BotState
  initBotState m = BotState { activeCommands  = initMap,
                              updateId        = 0,
                              token           = "",
                              manager         = m
                            }

  data ExecState = ExecState  { exprEnv     :: Map Expr,
                                tokenBot    :: String,
                                managerBot  :: Manager
                              }

  initExecState :: Manager -> Int -> String -> ExecState
  initExecState m chat token = ExecState  { exprEnv = mapFromList [("chat", Const (fromIntegral chat)), ("_", Const 0)],
                                            tokenBot = token,
                                            managerBot = m
                                          }
