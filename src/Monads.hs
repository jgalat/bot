module Monads where

  import Control.Monad.Trans.State
  import Control.Monad.Trans.Except

  import CommandAST (Type)
  import Environment

  type Check a = ExceptT String (State (Env Type)) a

  runChecker :: Check a -> Env Type -> Either String a
  runChecker c s = fst $ runState (runExceptT c) s
