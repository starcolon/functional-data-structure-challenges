module Data.Connection.Monad.GraphM where

import Data.Connection.Base.Graph(Gr, Node, Link, mapGr)

-- Graph is mappable
instance Functor Gr where
  fmap = mapGr

instance Applicative Gr

instance Monad Gr
