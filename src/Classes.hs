module Classes where

  import qualified Data.Map.Lazy as M

  type Key = String

  type Env v = M.Map Key v

  initEnv :: Env v
  initEnv = M.empty

  lookUp :: Key -> Env v -> Maybe v
  lookUp = M.lookup

  update :: (Key, v) -> Env v -> Env v
  update (k,v) e = M.insert k v e

  class Monad m => MonadState m where
    set       :: s -> m ()
    get       :: m s

  class Monad m => MonadError m where
    raise :: String -> m a
