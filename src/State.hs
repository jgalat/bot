module State where

  import Network.HTTP.Conduit (Manager)

  import Map
  import CommandAST

  initCheckMapList :: [(Key, ())]
  initCheckMapList = [("chat", ())]

  data BotState = BotState  { activeCommands  :: Map Comm,
                              updateId        :: Int,
                              token           :: String,
                              manager         :: Manager,
                              users           :: Map Int,
                              folder          :: Maybe String,
                              logFile         :: Maybe String
                            }
                | ExecState { exprEnv     :: Map Expr,
                              token       :: String,
                              manager     :: Manager,
                              users       :: Map Int,
                              logFile     :: Maybe String
                            }
                | DebugBotState { activeCommands  :: Map Comm,
                                  manager         :: Manager,
                                  users           :: Map Int,
                                  logFile         :: Maybe String
                                }
                | DebugExecState  { exprEnv :: Map Expr,
                                    manager :: Manager,
                                    users   :: Map Int,
                                    logFile :: Maybe String
                                  }

  initBotState :: Manager -> BotState
  initBotState m = BotState { activeCommands  = initMap,
                              updateId        = 0,
                              token           = "",
                              manager         = m,
                              users           = initMap,
                              folder          = Nothing,
                              logFile         = Nothing
                            }

  fromBotState :: BotState -> Int -> BotState
  fromBotState (s @ (BotState _ _ _ _ _ _ _)) chat = ExecState { exprEnv = mapFromList [("chat", Const (fromIntegral chat))],
                                                                 token   = token s,
                                                                 manager = manager s,
                                                                 users   = users s,
                                                                 logFile = logFile s
                                                               }
  fromBotState _ _ = error "Shouldn't happen (fromBotState)"

  initDebugBotState :: Manager -> BotState
  initDebugBotState m = DebugBotState { activeCommands  = initMap,
                                        manager         = m,
                                        users           = initMap,
                                        logFile         = Nothing
                                      }

  fromDebugBotState :: BotState -> BotState
  fromDebugBotState (s @ (DebugBotState _ m u lf)) = DebugExecState { exprEnv = mapFromList [("chat", DebugExpr)],
                                                                      manager = m,
                                                                      users   = u,
                                                                      logFile = lf
                                                                    }
  fromDebugBotState _ = error "Shouldn't happen (fromDebugBotState)"
