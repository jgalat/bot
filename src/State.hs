module State where

  import Network.HTTP.Conduit (Manager)

  import Map
  import CommandAST

  initCheckMapList :: [(Key, ())]
  initCheckMapList = [("chat", ()), ("_", ())]

  data BotState = BotState  { activeCommands  :: Map Comm,
                              updateId        :: Int,
                              token           :: String,
                              manager         :: Manager,
                              users           :: Map Int
                            }

  initBotState :: Manager -> BotState
  initBotState m = BotState { activeCommands  = initMap,
                              updateId        = 0,
                              token           = "",
                              manager         = m,
                              users           = initMap
                            }

  data ExecState = ExecState  { exprEnv     :: Map Expr,
                                tokenBot    :: String,
                                managerBot  :: Manager,
                                usersBot    :: Map Int
                              }

  initExecState :: Manager -> Int -> ExecState
  initExecState m chat = ExecState  { exprEnv = mapFromList [("chat", Const (fromIntegral chat)), ("_", Null)],
                                      tokenBot = "",
                                      managerBot = m,
                                      usersBot = initMap
                                    }
