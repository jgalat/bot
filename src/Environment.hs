{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Environment where

  import qualified Data.Map as M

  type Key = String

  type Env v = M.Map Key v

  initEnv :: Env v
  initEnv = M.empty

  lookUp :: Key -> Env v -> Maybe v
  lookUp = M.lookup

  lookUp' :: Key -> Env v -> v
  lookUp' k e = case lookUp k e of
                  Just v  -> v
                  _       -> error "This shouldn't happen if you knew it was already there..."

  update :: (Key, v) -> Env v -> Env v
  update (k,v) e = M.insert k v e

  envFromList :: [(Key, v)] -> Env v
  envFromList = M.fromList

  envToList :: Env v -> [(Key, v)]
  envToList = M.toList

  envDifference :: Env a -> Env b -> Env a
  envDifference = M.difference

  envUnion :: Env a -> Env a -> Env a
  envUnion = M.union
