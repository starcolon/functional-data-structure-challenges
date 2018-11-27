module Data.Connection.Base.Graph where

import qualified Data.Map as M

-- Graph of vertex type [v]
data G v = G ([V v]) (M.Map v (E v)) -- TAOTODO: Should implement a better tree-based list of [V v]
-- Vertex of type [v]
data V v = NV | V v Double
-- Edge of type [v]
data E v = NE | E v v Double

size :: G v -> Int
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
mapG f (G vs m) = G [fv f n | n <- vs] (M.fromList $ [(f a, fe f b) | (a,b) <- M.toList m])

pureG :: v -> G v
pureG v = error "TAOTODO:"