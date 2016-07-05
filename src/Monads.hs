{-# LANGUAGE FlexibleContexts #-}

module Monads where

  import Control.Monad.Trans.State
  import Control.Monad.Trans.Except
  import Control.Monad.State
  import Control.Monad.Except

  import CommandAST (Type)
  import Environment
  import State (BotState)

  raise :: (Monad m, MonadError String m) => String -> m a
  raise = throwError

  type Check a = ExceptT String (State (Env Type)) a

  runChecker :: Check a -> Env Type -> Either String a
  runChecker c s = fst $ runState (runExceptT c) s

  type Bot a = ExceptT String (StateT BotState IO) a

  runBot :: Bot a -> BotState -> IO (Either String a)
  runBot b s = runStateT (runExceptT b) s >>= (return . fst)
