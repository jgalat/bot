module Execute where

  import Data.List
  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class
  import Data.String.Conversions

  import CommandAST
  import Map
  import Monads (Bot, runExecution, raise)
  import TelegramAPI
  import qualified Communication as C
  import State (BotState (..))
  import Parser
  import PrettyPrint
  import Log


  execute :: [Expr] -> BotState -> Comm -> IO (Either String ())
  execute args st (Comm prmt c) =
    case cmpArgs args (map snd prmt) of
      Left err  -> return (Left err)
      _         -> let  envEx   = mapFromList $ zip (map fst prmt) args
                        execSt  = st { exprEnv = mapUnion (exprEnv st) envEx }
                   in   runExecution (evalComms c) execSt

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
      (Str s, _)            -> return (Str (s ++ showExpr e2))
      (_, Str s)            -> return (Str (showExpr e1 ++ s))
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
    let lf = logFile s
    case (e1, e2) of
      (Str msg, Const chat) -> do reply <- sendMessage' (manager s) (token s) (truncate chat) (unescape msg)
                                  case parseJSON (unescape $ cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> -- liftIO (logInfo lf ("Runtime warning\n" ++ e)) >> -- TODO
                                                 return (Str (cs reply))
      (_, Const chat)       -> do reply <- sendMessage' (manager s) (token s) (truncate chat) (showExpr e1)
                                  case parseJSON (cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> -- liftIO (logInfo lf ("Runtime warning\n" ++ e)) >> -- TODO
                                                 return (Str (cs reply))
      (Str msg, Str ('@':usr))  ->  case lookUp usr (users s) of
                                      Just chat -> do reply <- sendMessage' (manager s) (token s) chat (unescape msg)
                                                      case parseJSON (unescape $ cs reply) of
                                                        Ok r      -> return r
                                                        Failed e  -> -- liftIO (logInfo lf ("Runtime warning\n" ++ e)) >> -- TODO
                                                                     return (Str (cs reply))
                                      Nothing   -> return $ JsonObject (mapFromList [("ok", FalseExp)])
      (_, Str url)          -> do reply <- liftIO $ C.post (manager s) url (cs $ showExpr e1) -- TODO Test it well
                                  case parseJSON (cs reply) of
                                    Ok r      -> return r
                                    Failed e  -> -- liftIO (logInfo lf ("Runtime warning\n" ++ e)) >> -- TODO
                                                 return (Str (cs reply))
      _                     -> raise "Not implemented" -- TODO
  evalExpr (Get expr) = do
    s <- get
    e <- evalExpr expr
    let lf = logFile s
    case e of
      Str str -> do reply <- liftIO $ C.get (manager s) str
                    case parseJSON (cs reply) of
                      Ok r      -> return r
                      Failed e  -> -- liftIO (logInfo lf ("Runtime warning\n" ++ e)) >> -- TODO
                                   return (Str (cs reply))
      _       -> raise "Runtime error" -- TODO
  evalExpr x = return x
