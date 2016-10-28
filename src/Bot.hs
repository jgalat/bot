{-# LANGUAGE FlexibleContexts #-}

module Bot where

  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class
  import Control.Arrow (second)
  import Data.Maybe
  import Data.String.Conversions
  import Control.Exception (catch, IOException)
  import Data.ByteString.Lazy (ByteString)
  import System.Console.Readline hiding (message)

  import TelegramAPI
  import Map
  import CommandAST
  import Parser
  import State
  import Monads
  import qualified Communication as C
  import PrettyPrint
  import Log (logInfo)

  logBot :: String -> Bot ()
  logBot l = do s <- get
                liftIO (logInfo (logFile s) l)

  sendMessageBot :: Int -> String -> Bot ByteString
  sendMessageBot chat msg = do s <- get
                               liftIO (sendMessage' (manager s) (token s) chat msg)

  getUpdatesBot :: Bot (Maybe Reply)
  getUpdatesBot = do  s <- get
                      getUpdates (manager s) (token s) (updateId s)

  getUrlBot :: String -> Bot ByteString
  getUrlBot url = do  s <- get
                      liftIO (C.get (manager s) url)

  postUrlBot :: String -> ByteString -> Bot ByteString
  postUrlBot url p = do s <- get
                        liftIO (C.post (manager s) url p)

  debugBot :: Bot ()
  debugBot = do
    s <- get
    forever $ do
      line <- liftIO (readline "> ")
      case line of
        Just (_:_) -> do liftIO (addHistory (fromJust line))
                         case parseRequest (fromJust line) of
                           Ok (c, args) ->
                              case lookUp c (activeCommands s) of
                                Just cmd -> do
                                  e <- do logBot ("Executing " ++ c)
                                          let xdst = fromDebugBotState s
                                          execute args xdst cmd
                                  case e of
                                    Left err  -> logBot err
                                    _         -> logBot ("Finished execution of " ++ c)
                                Nothing  -> logBot ("Command (" ++ c ++ ") wasn't found.")
                           Failed err   -> logBot err
        _          -> return ()


  mainBot :: Bot ()
  mainBot =
    forever $ do
      s     <- get
      reply <- getUpdatesBot
      case reply of
        Nothing   -> logBot "Warning parse error"
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
                                              mapM_ (\(u,cid) ->
                                                  if isJust u
                                                  then  if cid >= 0
                                                        then logBot ("Received request from @" ++ fromJust u ++ " in private chat.")
                                                        else logBot ("Received request from @" ++ fromJust u ++ " in group chat.")
                                                  else  if cid >= 0
                                                        then logBot ("Received request from anonymous user in private chat.")
                                                        else logBot ("Received request from anonymous user in group chat.")) usrs
                                              execs <- mapM doRequest requestsOk
                                              s <- get
                                              mapM_ (\exec -> case exec of
                                                                Left err  -> logBot err
                                                                _         -> return ()) execs -- TODO
                                              put (s { updateId = update_id (last upds) + 1 })
                    _    -> logBot "Warning reply failed"

  doRequest :: (Int, (String, [Expr])) -> Bot (Either String ())
  doRequest (ch, ("feed", [Str name, Str url])) = do
        s <- get
        cont <- getUrlBot url
        let lf = logFile s
        case parseCommand (cs cont) of
          Ok comm    -> do
            logBot ("New command: " ++ name)
            let st = s { activeCommands = update (name, comm) (activeCommands s) }
            case folder s of
              Nothing  -> logBot (name ++ " wasn't saved. Only on memory.") >>
                          fmap Right (put st)
              Just fld -> let file = fld ++ name ++ ".comm"
                          in do
                            logBot ("Saving it in file " ++ file)
                            liftIO $ catch (writeFile file (cs cont))
                                  (\e -> let err = show (e :: IOException)
                                         in (logInfo lf (file ++ ": The file couldn't be opened.\n" ++ err)) >>
                                             logInfo lf (name ++ " wasn't saved. Only on memory."))
                            fmap Right (put st)
          Failed err -> return (Left ("feed: " ++ err))
  doRequest (ch, ("feed", _)) = do
        s <- get
        sendMessageBot ch "Usage: /feed name \"url\""
        return (Right ())
  doRequest (ch, (r, args)) = do
        s <- get
        case lookUp r (activeCommands s) of
          Just cmd -> do
            exec <-  do logBot ("Executing " ++ r)
                        let xst = fromBotState s ch
                        execute args xst cmd
            case exec of
              Left err -> return (Left (r ++ ": " ++ err))
              _        -> return exec -- TODO
          Nothing  -> return (Left ("Command (" ++ r ++ ") wasn't found."))


--
-- Execute
--

  execute :: [Expr] -> BotState -> Comm -> Bot (Either String ())
  execute args xst (Comm prmt c) = do
    case cmpArgs args (map snd prmt) of
      Left err  -> return (Left err)
      _         -> let  envEx   = mapFromList $ zip (map fst prmt) args
                        execSt  = xst { exprEnv = mapUnion (exprEnv xst) envEx }
                   in  liftIO (runBot (evalComms c) execSt)

  cmpArgs :: [Expr] -> [Type] -> Either String ()
  cmpArgs [] [] = Right ()
  cmpArgs [] _ = Left "Missing arguments"
  cmpArgs _ [] = Right ()
  cmpArgs (Const _ : xs) (Number : ts) = cmpArgs xs ts
  cmpArgs (Str _ : xs) (String : ts) = cmpArgs xs ts
  cmpArgs (TrueExp : xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (FalseExp: xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (Array _ : xs) (ArrayType : ts) = cmpArgs xs ts
  cmpArgs (JsonObject _ : xs) (JSON : ts) = cmpArgs xs ts
  -- TODO case where types do not match
  cmpArgs _ _ = Left "Arguments need to be normalized"

  evalComms :: [Statement] -> Bot ()
  evalComms = mapM_ evalComm

  evalComm :: Statement -> Bot ()
  evalComm (Assign var expr) = do
    e <- evalExpr expr
    s <- get
    put (s { exprEnv = update (var, e) (exprEnv s) })
  evalComm (If expr stmts) = do
    e <- evalExpr expr
    case e of
      TrueExp   -> evalComms stmts
      FalseExp  -> return ()
      _         -> raise "Runtime error" -- TODO
  evalComm (IfElse expr stmts1 stmts2) = do
    e <- evalExpr expr
    case e of
      TrueExp   -> evalComms stmts1
      FalseExp  -> evalComms stmts2
      _         -> raise "Runtime error" -- TODO
  evalComm (While expr stmts) = do
    e <- evalExpr expr
    case e of
      TrueExp   -> do evalComms stmts
                      evalComm (While expr stmts)
      FalseExp  -> return ()
      _         -> raise "Runtime error" -- TODO
  evalComm (Do stmts expr) = do
    evalComms stmts
    e <- evalExpr expr
    case e of
      TrueExp   -> evalComm (Do stmts expr)
      FalseExp  -> return ()
      _         -> raise "Runtime error" -- TODO
  evalComm (For v expr stmts) = do
    e <- evalExpr expr
    case e of
      Array a -> mapM_ (\ex -> do s <- get
                                  put (s { exprEnv = update (v, ex) (exprEnv s) })
                                  evalComms stmts) a
      Str a   -> mapM_ (\c -> do s <- get
                                 put (s { exprEnv = update (v, Str [c]) (exprEnv s) })
                                 evalComms stmts) a
      _       -> raise "Runtime error" -- TODO

  isNormal :: Expr -> Bool
  isNormal Null = True
  isNormal TrueExp = True
  isNormal FalseExp = True
  isNormal (Const _) = True
  isNormal (Str _ ) = True
  isNormal _ = False

  areNormal :: Expr -> Expr -> Bool
  areNormal e1 e2 = isNormal e1 && isNormal e2

  evalExpr :: Expr -> Bot Expr
  evalExpr (Var v) = do
    s <- get
    return (lookUp' v (exprEnv s))
  evalExpr (Not expr) = do
    e <- evalExpr expr
    case e of
      TrueExp   -> return FalseExp
      FalseExp  -> return TrueExp
      _         -> raise "Runtime error" -- TODO
  evalExpr (And expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (TrueExp, TrueExp)   -> return TrueExp
      (FalseExp, TrueExp)  -> return FalseExp
      (TrueExp, FalseExp)  -> return FalseExp
      (FalseExp, FalseExp) -> return FalseExp
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Or expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (FalseExp, FalseExp)  -> return FalseExp
      (FalseExp, TrueExp)   -> return TrueExp
      (TrueExp, FalseExp)   -> return TrueExp
      (TrueExp, TrueExp)    -> return TrueExp
      _                     -> raise "Runtime error" -- TODO
  evalExpr (Equals expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    if areNormal e1 e2 then if e1 == e2 then return TrueExp
                            else return FalseExp
    else raise "Runtime error" -- TODO
  evalExpr (Greater expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> if n1 > n2 then return TrueExp
                              else return FalseExp
      (Str s1, Str s2)     -> if s1 > s2 then return TrueExp
                              else return FalseExp
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Lower expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> if n1 < n2 then return TrueExp
                              else return FalseExp
      (Str s1, Str s2)     -> if s1 < s2 then return TrueExp
                              else return FalseExp
      _                    -> raise "Runtime error" -- TODO
  evalExpr (GreaterEquals expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> if n1 >= n2 then return TrueExp
                              else return FalseExp
      (Str s1, Str s2)     -> if s1 >= s2 then return TrueExp
                              else return FalseExp
      _                    -> raise "Runtime error" -- TODO
  evalExpr (LowerEquals expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> if n1 <= n2 then return TrueExp
                              else return FalseExp
      (Str s1, Str s2)     -> if s1 <= s2 then return TrueExp
                              else return FalseExp
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Negate expr) = do
    e <- evalExpr expr
    case e of
      Const n -> return (Const (-n))
      _       -> raise "Runtime error" -- TODO
  evalExpr (Plus expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2)  -> return (Const (n1 + n2))
      (Str s1, Str s2)      -> return (Str (s1 ++ s2))
      (Str s, Const n)      -> return (Str (s ++ showConst n))
      (Const n, Str s)      -> return (Str (showConst n ++ s))
      _                     -> raise "Runtime error" -- TODO
  evalExpr (Minus expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> return (Const (n1 - n2))
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Multiply expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (Const n1, Const n2) -> return (Const (n1 * n2))
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Divide expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (_, Const 0)         -> raise "Runtime error: Division by zero."
      (Const n1, Const n2) -> return (Const (n1 / n2))
      _                    -> raise "Runtime error" -- TODO
  evalExpr (Index expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    case (e1, e2) of
      (JsonObject o, Str k) -> case lookUp k o of
                                Nothing -> return Null
                                Just v  -> return v
      (Array [], Const _)   -> raise "Runtime error" -- TODO
      (Array a, Const n)    -> return (safeIndex a (truncate n))
      (Str "", Const _)     -> raise "Runtime error" -- TODO
      (Str s, Const n)      -> return (Str [(safeIndex s (truncate n))])
      _                     -> raise "Runtime error" -- TODO
    where safeIndex a n = let len = length a
                          in if n >= len then last a
                             else if n <= 0 then head a
                                  else a !! n
  evalExpr (JsonObject o) = let l = mapToList o
                                (f,s) = unzip l
                            in do
                              el <- mapM evalExpr s
                              return (JsonObject (mapFromList $ zip f el))
  evalExpr (Array exprs)  = fmap Array (mapM evalExpr exprs)
  evalExpr (Post expr1 expr2) = do
    e1 <- evalExpr expr1
    e2 <- evalExpr expr2
    s  <- get
    case (e1, e2) of
      (Str msg, DebugExpr)  -> do logBot ("Bot:\n" ++ msg)
                                  return Null
      (_, DebugExpr)        -> do logBot ("Bot:\n" ++ showExprJSONValid e1)
                                  return Null
      (Str msg, Const chat) -> do reply <- sendMessageBot (truncate chat) (unescape msg)
                                  case parseJSON (unescape $ cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> logBot ("Runtime warning\n" ++ e) >> -- TODO
                                                 return (Str (cs reply))
      (_, Const chat)       -> do reply <- sendMessageBot (truncate chat) (showExprJSONValid e1)
                                  case parseJSON (cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> logBot ("Runtime warning\n" ++ e) >> -- TODO
                                                 return (Str (cs reply))
      (Str msg, Str ('@':usr))  ->  case lookUp usr (users s) of
                                      Just chat -> do reply <- sendMessageBot chat (unescape msg)
                                                      case parseJSON (unescape $ cs reply) of
                                                        Ok r      -> return r
                                                        Failed e  -> logBot ("Runtime warning\n" ++ e) >> -- TODO
                                                                     return (Str (cs reply))
                                      Nothing   -> return $ JsonObject (mapFromList [("ok", FalseExp)])
      (_, Str url)          -> do reply <- postUrlBot url (cs $ showExprJSONValid e1) -- TODO Test it well
                                  case parseJSON (cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> logBot ("Runtime warning\n" ++ e) >> -- TODO
                                                 return (Str (cs reply))
      _                     -> raise "Not implemented" -- TODO
  evalExpr (Get expr) = do
    e <- evalExpr expr
    case e of
      Str str -> do reply <- getUrlBot str
                    case parseJSON (cs reply) of
                      Ok r      -> return r
                      Failed e  -> logBot ("Runtime warning\n" ++ e) >> -- TODO
                                   return (Str (cs reply))
      _       -> raise "Runtime error" -- TODO
  evalExpr x = return x
