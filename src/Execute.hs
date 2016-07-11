module Execute where

  import Data.List
  import Control.Monad.State
  import Control.Monad.Except
  import Control.Monad.IO.Class
  import Data.String.Conversions

  import CommandAST
  import Environment
  import Monads (Execution, runExecution, raise)
  import TelegramAPI
  import qualified Communication as C
  import State (ExecState (..))
  import Parser
  import PrettyPrint


  execute :: [Expr] -> ExecState -> Comm -> IO (Either String ())
  execute args st (Comm prmt c) = case cmpArgs args (map snd prmt) of
                                    Left err  -> return (Left err)
                                    _         -> let  envEx   = envFromList $ zip (map fst prmt) args
                                                      execSt  = st { exprEnv = envUnion (exprEnv st) envEx }
                                                 in   runExecution (evalComms c) execSt

  cmpArgs :: [Expr] -> [Type] -> Either String ()
  cmpArgs [] [] = Right ()
  cmpArgs [] _ = Left "Error" -- TODO
  cmpArgs _ [] = Left "Error" -- TODO
  cmpArgs (Const _ : xs) (Number : ts) = cmpArgs xs ts
  cmpArgs (Str _ : xs) (String : ts) = cmpArgs xs ts
  cmpArgs (TrueExp : xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (FalseExp: xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (JsonArray _ : xs) (JSON : ts) = cmpArgs xs ts
  cmpArgs (JsonObject _ : xs) (JSON : ts) = cmpArgs xs ts
  cmpArgs _ _ = Left "Error" -- TODO

  evalComms :: [Statement] -> Execution ()
  evalComms = mapM_ evalComm

  evalComm :: Statement -> Execution ()
  evalComm (Declaration var expr) = do  e <- evalExpr expr
                                        s <- get
                                        put (s { exprEnv = update (var, e) (exprEnv s) })
  evalComm (Assign var expr) = do e <- evalExpr expr
                                  s <- get
                                  put (s { exprEnv = update (var, e) (exprEnv s) })
  evalComm (If expr stmts) = do e <- evalExpr expr
                                case e of
                                  TrueExp   -> evalComms stmts
                                  FalseExp  -> return ()
  evalComm (IfElse expr stmts1 stmts2) = do e <- evalExpr expr
                                            case e of
                                              TrueExp   -> evalComms stmts1
                                              FalseExp  -> evalComms stmts2
  evalComm (While expr stmts) = do  e <- evalExpr expr
                                    case e of
                                      TrueExp   -> do evalComms stmts
                                                      evalComm (While expr stmts)
                                      FalseExp  -> return ()
  evalComm (Do stmts expr) = do evalComms stmts
                                e <- evalExpr expr
                                case e of
                                  TrueExp   -> evalComm (Do stmts expr)
                                  FalseExp  -> return ()

  evalExpr :: Expr -> Execution Expr
  evalExpr (Var v) = do s <- get
                        return (lookUp' v (exprEnv s))
  evalExpr (Not expr) = do  e <- evalExpr expr
                            case e of
                              TrueExp   -> return FalseExp
                              FalseExp  -> return TrueExp
  evalExpr (And expr1 expr2) = do e1 <- evalExpr expr1
                                  e2 <- evalExpr expr2
                                  case (e1, e2) of
                                    (TrueExp, TrueExp) -> return TrueExp
                                    _                  -> return FalseExp
  evalExpr (Or expr1 expr2) = do  e1 <- evalExpr expr1
                                  e2 <- evalExpr expr2
                                  case (e1, e2) of
                                    (FalseExp, FalseExp)  -> return FalseExp
                                    _                     -> return TrueExp
  evalExpr (Equals expr1 expr2) = do  e1 <- evalExpr expr1
                                      e2 <- evalExpr expr2
                                      case (e1, e2) of
                                        (Const n1, Const n2) -> if (n1 == n2)
                                                                then return TrueExp
                                                                else return FalseExp
                                        (Str s1, Str s2)     -> if (s1 == s2)
                                                                then return TrueExp
                                                                else return FalseExp
  evalExpr (Greater expr1 expr2) = do e1 <- evalExpr expr1
                                      e2 <- evalExpr expr2
                                      case (e1, e2) of
                                        (Const n1, Const n2) -> if (n1 > n2)
                                                                then return TrueExp
                                                                else return FalseExp
                                        (Str s1, Str s2)     -> if (s1 > s2)
                                                                then return TrueExp
                                                                else return FalseExp
  evalExpr (Lower expr1 expr2) = do e1 <- evalExpr expr1
                                    e2 <- evalExpr expr2
                                    case (e1, e2) of
                                      (Const n1, Const n2) -> if (n1 < n2)
                                                              then return TrueExp
                                                              else return FalseExp
                                      (Str s1, Str s2)     -> if (s1 < s2)
                                                              then return TrueExp
                                                              else return FalseExp
  evalExpr (GreaterEquals expr1 expr2) = do e1 <- evalExpr expr1
                                            e2 <- evalExpr expr2
                                            case (e1, e2) of
                                              (Const n1, Const n2) -> if (n1 >= n2)
                                                                      then return TrueExp
                                                                      else return FalseExp
                                              (Str s1, Str s2)     -> if (s1 >= s2)
                                                                      then return TrueExp
                                                                      else return FalseExp
  evalExpr (LowerEquals expr1 expr2) = do e1 <- evalExpr expr1
                                          e2 <- evalExpr expr2
                                          case (e1, e2) of
                                            (Const n1, Const n2) -> if (n1 <= n2)
                                                                    then return TrueExp
                                                                    else return FalseExp
                                            (Str s1, Str s2)     -> if (s1 <= s2)
                                                                    then return TrueExp
                                                                    else return FalseExp
  evalExpr (Negate expr) = do e <- evalExpr expr
                              case e of
                                Const n -> return (Const (-n))
  evalExpr (Plus expr1 expr2) = do  e1 <- evalExpr expr1
                                    e2 <- evalExpr expr2
                                    case (e1, e2) of
                                      (Const n1, Const n2) -> return (Const (n1 + n2))
                                      (Str s1, Str s2) -> return (Str (s1 ++ s2))
                                      (Str s, _) -> return (Str (s ++ (showExpr e2)))
                                      (_, Str s) -> return (Str ((showExpr e1) ++ s))
  evalExpr (Minus expr1 expr2) = do e1 <- evalExpr expr1
                                    e2 <- evalExpr expr2
                                    case (e1, e2) of
                                      (Const n1, Const n2) -> return (Const (n1 - n2))
  evalExpr (Multiply expr1 expr2) = do  e1 <- evalExpr expr1
                                        e2 <- evalExpr expr2
                                        case (e1, e2) of
                                          (Const n1, Const n2) -> return (Const (n1 * n2))
  evalExpr (Divide expr1 expr2) = do  e1 <- evalExpr expr1
                                      e2 <- evalExpr expr2
                                      case (e1, e2) of
                                        (_, Const 0)         -> raise "Division by zero."
                                        (Const n1, Const n2) -> return (Const (n1 / n2))
  evalExpr (Index expr1 expr2) = do e1 <- evalExpr expr1
                                    e2 <- evalExpr expr2
                                    case (e1, e2) of
                                      (JsonObject o, Str k) -> case lookUp k o of
                                                                Nothing -> raise ("Key "++ k ++ " wasn't found.")
                                                                Just v  -> return v
  evalExpr (JsonObject o) = let l = envToList o
                                (f,s) = unzip l
                            in do el <- mapM evalExpr s
                                  return (JsonObject (envFromList $ zip f el))
  evalExpr (JsonArray exprs) = mapM evalExpr exprs >>= return . JsonArray
  evalExpr (Post expr1 expr2) = do  e1 <- evalExpr expr1
                                    e2 <- evalExpr expr2
                                    s  <- get
                                    case (e1, e2) of
                                      (Str msg, Const chat) -> do reply <- sendMessage' (managerBot s) (tokenBot s) (truncate chat) (unescape msg)
                                                                  case parseJSON (unescape $ cs reply) of
                                                                    Ok r        -> return r
                                                                    Failed err  -> raise err
                                      (_, Const chat)       -> do reply <- sendMessage' (managerBot s) (tokenBot s) (truncate chat) (showExpr e1)
                                                                  case parseJSON (cs reply) of
                                                                    Ok r        -> return r
                                                                    Failed err  -> raise err
                                      (Str _, Str ('@':_))  -> raise "Not implemented"
                                      (_, Str url)          -> do reply <- liftIO $ C.post (managerBot s) url (cs $ showExpr e1) -- TODO Test it well
                                                                  case parseJSON (cs reply) of
                                                                    Ok r        -> return r
                                                                    Failed err  -> raise err
                                      _                     -> raise "Not implemented"
  evalExpr (Get expr) = do  s <- get
                            e <- evalExpr expr
                            case e of
                              Str str -> do reply <- liftIO $ C.get (managerBot s) str
                                            case parseJSON (cs reply) of
                                              Ok r        -> return r
                                              Failed err  -> raise err
  evalExpr x = return x
