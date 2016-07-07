{-# LANGUAGE FlexibleContexts #-}

module Monads where

  import Control.Monad.Trans.State
  import Control.Monad.Trans.Except
  import Control.Monad.State
  import Control.Monad.Except

  import CommandAST (Type, Expr)
  import Environment
  import State (BotState, ExecState)

  raise :: (Monad m, MonadError String m) => String -> m a
  raise = throwError

  type Check a = ExceptT String (State (Env Type)) a

  runChecker :: Check a -> Env Type -> (Either String a, Env Type)
  runChecker c s = runState (runExceptT c) s

  type Bot a = ExceptT String (StateT BotState IO) a

  runBot :: Bot a -> BotState -> IO (Either String a)
  runBot b s = runStateT (runExceptT b) s >>= (return . fst)

  type Execution a = ExceptT String (StateT ExecState IO) a

  runExecution :: Execution a -> ExecState -> IO (Either String a)
  runExecution e s = runStateT (runExceptT e) s >>= (return . fst)
