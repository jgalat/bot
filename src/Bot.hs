{-# LANGUAGE FlexibleContexts #-}

module Bot where

  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class
  import Control.Arrow (second)
  import Data.Maybe
  import Data.String.Conversions
  import Control.Exception (catch, IOException)

  import TelegramAPI
  import Map
  import CommandAST
  import Parser (ParseResult(..), parseRequest, parseCommand)
  import State (BotState (..), initExecState)
  import Monads (Bot, raise)
  import qualified Communication as C
  import Execute
  import Log

  echoBot :: Bot ()
  echoBot = do
    s <- get
    let m = manager s
    let t = token s
    forever $ do
      s     <- get
      reply <- getUpdates m t (updateId s)
      case reply of
        Nothing   -> return ()
        Just rep  -> case ok rep of
                      True -> case updates rep of
                                []  -> liftIO (putStrLn "No Updates...") >> echoBot
                                xs  -> do liftIO (putStrLn "New Updates!")
                                          mapM_ (\x -> do liftIO (putStrLn "Replying...")
                                                          let chatId = (chat_id . chat . message) x
                                                          let messg = fromMaybe "(null)" ((text . message) x)
                                                          sendMessage m t chatId messg) xs
                                          put (s { updateId = update_id (last xs) + 1 })
                      _    -> return ()

  mainBot :: Bot ()
  mainBot = do
    s   <- get
    let lf = logFile s
    let m  = manager s
    let t  = token s
    forever $ do
      s     <- get
      reply <- getUpdates m t (updateId s)
      case reply of
        Nothing   -> liftIO (logInfo lf "Warning parse error")
        Just rep  -> case ok rep of
                    True -> case updates rep of
                            []    -> return ()
                            upds  -> let  (usrs, requests) = unzip $ map (\u ->
                                                        let chatId = (chat_id . chat . message) u
                                                            messg = fromMaybe "" ((text . message) u)
                                                            usernm = (username . from . message) u
                                                        in ((usernm, chatId), (chatId, messg))) upds
                                          newUsers = foldr (\(usr, ch) u ->
                                                              if isJust usr && ch >= 0 then update (fromJust usr, ch) u
                                                              else u) (users s) usrs
                                          st = s { users = newUsers }
                                          parsedRequests = map (second parseRequest) requests
                                          requestsOk =  map (\(c, Ok r) -> (c, r)) $
                                                        filter (\(_, r) -> case r of
                                                                            Ok _ -> True
                                                                            _    -> False) parsedRequests
                                      in  do  put st
                                              liftIO $ mapM_ (\(u,cid) -> if isJust u
                                                                          then  if cid >= 0
                                                                                then logInfo lf ("Received request from @" ++ fromJust u ++ " in private chat.")
                                                                                else logInfo lf ("Received request from @" ++ fromJust u ++ " in group chat.")
                                                                          else  if cid >= 0
                                                                                then logInfo lf ("Received request from anonymous user in private chat.")
                                                                                else logInfo lf ("Received request from anonymous user in group chat.")) usrs
                                              execs <- mapM doRequest requestsOk
                                              s <- get
                                              mapM_ (\exec -> case exec of
                                                                Left err  -> liftIO $ logInfo lf err
                                                                _         -> return ()) execs -- TODO
                                              put (s { updateId = update_id (last upds) + 1 })
                    _    -> liftIO (logInfo lf "Warning reply failed")

  doRequest :: (Int, (String, [Expr])) -> Bot (Either String ())
  doRequest (ch, ("feed", [Str name, Str url])) = do
        s <- get
        cont <- liftIO $ C.get (manager s) url
        let lf = logFile s
        case parseCommand (cs cont) of
          Ok comm    -> do
                        liftIO $ logInfo lf ("New command: " ++ name)
                        let st = s { activeCommands = update (name, comm) (activeCommands s) }
                        case folder s of
                          Nothing  -> (liftIO $ logInfo lf (name ++ " wasn't saved. Only on memory.")) >>
                                      fmap Right (put st)
                          Just fld -> let file = fld ++ name ++ ".comm"
                                      in do
                                        liftIO $ logInfo lf ("Saving it in file " ++ file)
                                        liftIO $ catch (writeFile file (cs cont))
                                              (\e -> let err = show (e :: IOException)
                                                     in (logInfo lf (file ++ ": The file couldn't be opened.\n" ++ err)) >>
                                                        logInfo lf (name ++ " wasn't saved. Only on memory."))
                                        fmap Right (put st)
          Failed err -> return (Left ("feed: " ++ err))
  doRequest (ch, ("feed", _)) = do
        s <- get
        liftIO (sendMessage' (manager s) (token s) ch "Usage: /feed name \"url\"")
        return (Right ())
  doRequest (ch, (r, args)) = do
        s <- get
        let lf = logFile s
        case lookUp r (activeCommands s) of
          Just cmd -> do
            exec <- liftIO $ logInfo lf ("Executing " ++ r) >>
                             execute args ((initExecState (manager s) ch) { users   = users s,
                                                                            token   = token s,
                                                                            logFile = lf
                                                                          }) cmd
            case exec of
              Left err -> return (Left (r ++ ": " ++ err))
              _        -> return exec -- TODO
          Nothing  -> return (Left ("Command (" ++ r ++ ") wasn't found."))
