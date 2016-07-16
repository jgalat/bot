{-# LANGUAGE FlexibleContexts #-}

module Monads where

  import Control.Monad.Trans.State
  import Control.Monad.Trans.Except
  import Control.Monad.State
  import Control.Monad.Except

  import CommandAST (Expr)
  import Map
  import State (BotState, ExecState)

  raise :: (Monad m, MonadError String m) => String -> m a
  raise = throwError

  type Check a = ExceptT String (State (Map ())) a

  runChecker :: Check a -> Map () -> Either String a
  runChecker c s = fst (runState (runExceptT c) s)

  type Bot a = ExceptT String (StateT BotState IO) a

  runBot :: Bot a -> BotState -> IO (Either String a)
  runBot b s = runStateT (runExceptT b) s >>= (return . fst)

  type Execution a = ExceptT String (StateT ExecState IO) a

  runExecution :: Execution a -> ExecState -> IO (Either String a)
  runExecution e s = runStateT (runExceptT e) s >>= (return . fst)
