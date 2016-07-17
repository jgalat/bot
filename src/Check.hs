{-# LANGUAGE FlexibleContexts #-}

module Check where

  import Control.Monad.State
  import Control.Monad.Except

  import CommandAST
  import Map
  import State (initCheckMapList)
  import Monads (Check, runChecker, raise)

  check :: Comm -> Either String Comm
  check (Comm p c)  = let s = mapFromList $ initCheckMapList ++ map (\(x,_) -> (x,())) p
                      in case runChecker (checkStmts c) s of
                          Left err -> Left err
                          _        -> Right (Comm p c)


  checkStmts :: (Monad m, MonadError String m, MonadState (Map ()) m) => [Statement] -> m ()
  checkStmts = mapM_ checkStmt

  checkStmt :: (Monad m, MonadError String m, MonadState (Map ()) m) => Statement -> m ()
  checkStmt (Assign v e) = do checkExpr e
                              s <- get
                              put (update (v,()) s)
  checkStmt (If e stmts) = do checkExpr e
                              checkStmts stmts
  checkStmt (IfElse e stmts stmts') = do  checkExpr e
                                          s <- get
                                          checkStmts stmts
                                          put s
                                          checkStmts stmts'
                                          put s
  checkStmt (While e stmts) = do  checkExpr e
                                  checkStmts stmts
  checkStmt (Do stmts e) = do checkExpr e
                              checkStmts stmts
  checkStmt (For v e stmts) = do  s <- get
                                  checkExpr e
                                  put (update (v,()) s)
                                  checkStmts stmts
                                  put s

  checkExpr :: (Monad m, MonadError String m, MonadState (Map ()) m) => Expr -> m ()
  checkExpr (Var v) = do  s <- get
                          case lookUp v s of
                            Nothing -> raise "Error" -- TODO
                            Just _  -> return ()
  checkExpr (Not e) = checkExpr e
  checkExpr (And e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Or e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Equals e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Greater e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Lower e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (GreaterEquals e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (LowerEquals e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Negate e) = checkExpr e
  checkExpr (Plus e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Minus e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Multiply e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Divide e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Index e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (Get e) = checkExpr e
  checkExpr (Post e1 e2) = checkExpr e1 >> checkExpr e2
  checkExpr (JsonObject o) = mapM_ (checkExpr . snd) (mapToList o)
  checkExpr (Array e) = mapM_ checkExpr e
  checkExpr _ = return ()
