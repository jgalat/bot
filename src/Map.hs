module Map where

  import qualified Data.Map as M

  type Key = String

  type Map v = M.Map Key v

  initMap :: Map v
  initMap = M.empty

  lookUp :: Key -> Map v -> Maybe v
  lookUp = M.lookup

  lookUp' :: Key -> Map v -> v
  lookUp' k e = case lookUp k e of
                  Just v  -> v
                  _       -> error ("Key \""++ k ++ "\" is missing.")

  update :: (Key, v) -> Map v -> Map v
  update (k,v) = M.insert k v

  mapFromList :: [(Key, v)] -> Map v
  mapFromList = M.fromList

  mapToList :: Map v -> [(Key, v)]
  mapToList = M.toList

  mapDifference :: Map a -> Map b -> Map a
  mapDifference = M.difference

  mapUnion :: Map a -> Map a -> Map a
  mapUnion = M.union
