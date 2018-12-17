module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

-- Graph of type [v] implements Vertices of type [v]

data G v = G (M.Map v (V v)) deriving (Show,Eq)

-- Vertex consists of a value [v], weight, and a Map of edges
data V v = NV | V v Double (M.Map v Double)  deriving (Show,Eq)

size :: Ord v => G v -> Int
size (G n) = M.size n

-- Associativity of vertex
-- f (V v) = V (f v)
fv :: (v -> v') -> V v -> V v'
fv f v = case v of 
  NV  -> NV
  V a d m -> V a' d m'
    where 
      a' = f a
      m' = M.fromDistinctAscList $ [(f k,v) | (k,v) <- M.toList m]

-- Join two graphs
(<+>) :: Ord v => G v -> G v -> G v
(<+>) (G va) (G vb) = G (M.fromDistinctAscList $ [(v, blendVertices v va vb) | v <- S.toList keys])
  where 
    keys = S.fromList $ (M.keys va) ++ (M.keys vb)

-- Blend the specified vertex from the two maps
blendVertices :: Ord v => v -> M.Map v (V v) -> M.Map v (V v) -> V v
blendVertices a ma mb = case (M.lookup a ma, M.lookup a mb) of 
  (Nothing, Nothing) -> NV
  (Nothing, v) -> v
  (v, Nothing) -> v
  -- NOTE: following v1,d1 and v2,d2 are identical in all possible cases
  (Just V v1 d1 m1, Just v2 d2 m2) -> V v1 d1 (M.union m1 m2)

has :: Ord v => G v -> v -> Bool
has (G n m) v = M.member v m

ensureVertex :: Ord v => V v -> G v -> G v
ensureVertex NV g = g
ensureVertex (V a d) (G n m) = case M.lookup a m of
  Nothing -> G n' m' 
    where 
      v' = V a d
      n' = M.insert a v' n
      m' = M.insert a [] m
  Just _ -> G n m

mapG :: (v -> v') -> G v -> G v'
mapG f (G n m) = 
  let n' = M.fromDistinctAscList $ [(f a, V (f a') d) | (a,V a' d) <- M.toList n]
      m' = M.fromDistinctAscList $ [(f a, [fe f b | b <- bs]) | (a,bs) <- M.toList m]
  in G n' m'

-- Create a unit graph with an initial vertex
pureG :: v -> G v
pureG v = G n m
  where 
    n = M.singleton v (V v 1)
    m  = M.singleton v []

flatMapG :: G v -> (v -> G v') -> G v'
flatMapG _ _ = error "TAOTODO:"
-- flatMapG (G n m) f = G nf' mf' 
--   where n'  = [ | n <- M.toList n ]
--         nf' = ???
--         mf' = ???


