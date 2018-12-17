module Data.Connection.Monad.GraphM where

import Data.Connection.Base.Graph(G, E, V, mapG, pureG, apG, flatMapG)

-- Graph is mappable
instance Functor G where
  fmap = mapG

instance Applicative G where
  pure = pureG

-- Graph is flatmappable
instance Monad G where
  return = pureG
  (>>=) = flatMapG
