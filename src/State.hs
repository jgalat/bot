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

  initBotState :: Manager -> BotState
  initBotState m = BotState { activeCommands  = initMap,
                              updateId        = 0,
                              token           = "",
                              manager         = m,
                              users           = initMap,
                              folder          = Nothing,
                              logFile         = Nothing
                            }

  initExecState :: Manager -> Int -> BotState
  initExecState m chat = ExecState  { exprEnv   = mapFromList [("chat", Const (fromIntegral chat)), ("_", Null)],
                                      token     = "",
                                      manager   = m,
                                      users     = initMap,
                                      logFile   = Nothing
                                    }
