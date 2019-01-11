module Data.Connection.Monad.GraphM where

import Data.Connection.Base.Graph(
  G,
  pureG,
  mapG,
  unitG,
  flatMapG)

-- Graph is mappable
instance Functor G where
  fmap = mapG

instance Applicative G where
  pure = unitG