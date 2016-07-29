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

  echoBot :: Bot ()
  echoBot = do
    s     <- get
    reply <- getUpdates (manager s) (token s) (updateId s)
    case reply of
      Nothing   -> raise "Error Parsing!"
      Just rep  -> case ok rep of
                    True -> case updates rep of
                              []  -> liftIO (putStrLn "No Updates...") >> echoBot
                              xs  -> do liftIO (putStrLn "New Updates!")
                                        mapM_ (\x -> do liftIO (putStrLn "Replying...")
                                                        let chatId = (chat_id . chat . message) x
                                                        let messg = fromMaybe "(null)" ((text . message) x)
                                                        sendMessage (manager s) (token s) chatId messg) xs
                                        Right <$> put (s { updateId = update_id (last xs) + 1 })
                                        echoBot
                    _    -> echoBot

  mainBot :: Bot ()
  mainBot = do
    s     <- get
    reply <- getUpdates (manager s) (token s) (updateId s)
    case reply of
      Nothing   -> raise "Error Parsing!" -- TODO
      Just rep  -> case ok rep of
                  True -> case updates rep of
                          []    -> mainBot
                          upds  -> let  (usrs, requests) = unzip $ map (\u ->
                                                      let chatId = (chat_id . chat . message) u
                                                          messg = fromMaybe "" ((text . message) u)
                                                          usernm = if chatId < 0 then Nothing
                                                                   else (username . from . message) u
                                                      in ((usernm, chatId), (chatId, messg))) upds
                                        newUsers = foldr (\(usr, ch) u ->
                                                            if isJust usr then update (fromJust usr, ch) u
                                                            else u) (users s) usrs
                                        st = s { users = newUsers }
                                        parsedRequests = map (second parseRequest) requests
                                        requestsOk =  map (\(c, Ok r) -> (c, r)) $
                                                      filter (\(_, r) -> case r of
                                                                          Ok _ -> True
                                                                          _    -> False) parsedRequests
                                    in  do  put st
                                            execs <- mapM doRequest requestsOk
                                            s <- get
                                            mapM_ (\exec -> case exec of
                                                              Left err  -> liftIO $ putStrLn err
                                                              _         -> return ()) execs -- TODO
                                            put (s { updateId = update_id (last upds) + 1 })
                                            mainBot
                  _    -> liftIO (putStrLn "Failed rep!") >> mainBot

  doRequest :: (Int, (String, [Expr])) -> Bot (Either String ())
  doRequest (ch, ("feed", [Str name, Str url])) = do
                  s <- get
                  cont <- liftIO $ C.get (manager s) url
                  case parseCommand (cs cont) of
                    Ok comm    -> let st = s { activeCommands = update (name, comm) (activeCommands s) }
                                  in case folder s of
                                    Nothing  -> fmap Right (put st)
                                    Just fld -> let file = fld ++ name ++ ".comm"
                                                in do
                                                  liftIO $ catch (writeFile file (cs cont))
                                                        (\e -> let err = show (e :: IOException)
                                                               in putStrLn $ file ++ ": The file couldn't be opened.\n" ++ err)
                                                  fmap Right (put st)
                    Failed err -> return (Left ("feed: " ++ err))
  doRequest (ch, ("feed", _)) = do s <- get
                                   liftIO (sendMessage' (manager s) (token s) ch "Usage: /feed name \"url\"")
                                   return (Right ())
  doRequest (ch, (r, args)) = do
                  s <- get
                  case lookUp r (activeCommands s) of
                    Just cmd -> do
                      exec <- liftIO $ execute args ((initExecState (manager s) ch) { users = users s,
                                                                                      token = token s
                                                                                    }) cmd
                      case exec of
                        Left err -> return (Left (r ++ ": " ++ err))
                        _        -> return exec -- TODO
                    Nothing  -> return (Left (r ++ ": not found"))
