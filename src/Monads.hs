{-# LANGUAGE FlexibleContexts #-}

module Monads where

  import Control.Monad.Trans.State
  import Control.Monad.Trans.Except
  import Control.Monad.State
  import Control.Monad.Except

  import CommandAST (Expr)
  import Map
  import State (BotState)

  raise :: (Monad m, MonadError String m) => String -> m a
  raise = throwError

  type Check a = ExceptT String (State (Map ())) a

  runChecker :: Check a -> Map () -> Either String a
  runChecker c = evalState (runExceptT c)

  type Bot a = ExceptT String (StateT BotState IO) a

  runBot :: Bot a -> BotState -> IO (Either String a)
  runBot b s = fmap fst (runStateT (runExceptT b) s)

  runExecution :: Bot a -> BotState -> IO (Either String a)
  runExecution = runBot
