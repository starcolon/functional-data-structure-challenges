module Data.Connection.Base.Graph where

import qualified Data.Map as M

-- Graph of type [v] implements Vertices of type [v]

data G v = G [V v] (M.Map v (E v)) deriving Show-- TAOTODO: Should implement a better tree-based list of [V v]

data V v = NV | V v Double deriving Show

data E v = NE | E v v Double deriving Show

size :: Ord v => G v -> Int
size (G ns _) = length $ ns

-- Associativity of vertex
-- f (V v) = V (f v)
fv :: (v -> v') -> V v -> V v'
fv f v = case v of 
  NV  -> NV
  V a d -> error "TAOTODO:"

-- Associativity of edge
-- f (E v) = E (f e)
fe :: (v -> v') -> E v -> E v'
fe f e = case e of 
  NE    -> NE
  E a b d -> error "TAOTODO:"

mapG :: (v -> v') -> G v -> G v'
mapG f (G vs m) = G [fv f n | n <- vs] (M.fromDistinctAscList $ [(f a, fe f b) | (a,b) <- M.toList m])

-- Create a unit graph with an initial vertex
pureG :: v -> G v
pureG v = error "TAOTODO:"