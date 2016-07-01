{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Classes where

  import Control.Monad.IO.Class
  import Control.Applicative (Applicative(..))
  import Control.Monad       (liftM, ap)
  import qualified Data.Map as M
  import CommandAST
  import Control.Monad.IO.Class

  type Key = String

  type Env v = M.Map Key v

  initEnv :: Env v
  initEnv = M.empty

  lookUp :: Key -> Env v -> Maybe v
  lookUp = M.lookup

  update :: (Key, v) -> Env v -> Env v
  update (k,v) e = M.insert k v e

  class Monad m => MonadState s m | m -> s where -- VER
    get       :: m s
    set       :: s -> m ()

  class Monad m => MonadError m where
    raise :: String -> m a

  newtype StateWE s a = StateWE { runStateWE :: s -> Either String (a, s) }

  instance Monad (StateWE s) where
    return x         = StateWE (\s -> Right (x, s))
    StateWE f >>= g  = StateWE (\s -> case f s of
                                        Left str -> Left str
                                        Right (x, s') -> runStateWE (g x) s')

  instance MonadState s (StateWE s) where
    get   = StateWE (\s -> Right (s, s))
    set s = StateWE (\_ -> Right ((), s))

  instance MonadError (StateWE s) where
    raise str = StateWE (\_ -> Left str)

  instance Functor (StateWE s) where
    fmap = liftM

  instance Applicative (StateWE s) where
    pure  = return
    (<*>) = ap
