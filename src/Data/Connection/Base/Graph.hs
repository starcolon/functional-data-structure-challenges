module Data.Connection.Base.Graph where

import qualified Data.Map as M

-- Graph of type [v] implements Vertices of type [v]

data G v = G [V v] (M.Map v (E v)) deriving (Show,Eq) -- TAOTODO: Should implement a better tree-based list of [V v]

data V v = NV | V v Double deriving (Show,Eq)

data E v = NE | E v v Double deriving (Show,Eq)

size :: Ord v => G v -> Int
size (G ns _) = length $ ns

-- Associativity of vertex
-- f (V v) = V (f v)
fv :: (v -> v') -> V v -> V v'
fv f v = case v of 
  NV  -> NV
  V a d -> V a' d where a' = f a

-- Associativity of edge
-- f (E v) = E (f e)
fe :: (v -> v') -> E v -> E v'
fe f e = case e of 
  NE    -> NE
  E a b d -> E a' b' d 
    where 
      a' = f a
      b' = f b

mapG :: (v -> v') -> G v -> G v'
mapG f (G vs m) = G [fv f n | n <- vs] (M.fromDistinctAscList $ [(f a, fe f b) | (a,b) <- M.toList m])

-- Create a unit graph with an initial vertex
pureG :: v -> G v
pureG v = G vs m
  where 
    vs = [V v 1]
    m  = M.singleton v NE


