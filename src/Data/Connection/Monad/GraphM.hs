module Data.Connection.Monad.GraphM where

import Data.Connection.Base.Graph(G, V, mapG, unitG, flatMapG)

-- Graph is mappable
instance Functor G where
  fmap = mapG

instance Applicative G where
  pure = unitG

-- Graph is flatmappable
instance Monad G where
  return = pureG
  (>>=) = flatMapG
