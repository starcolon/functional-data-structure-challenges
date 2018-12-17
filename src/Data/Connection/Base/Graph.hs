module Data.Connection.Base.Graph where

import qualified Data.Set as S
import qualified Data.Map as M

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

-- Apply a function to the vertex
(<**>) :: V v -> (v -> v') -> V v'
(<**>) NV _ = NV
(<**>) (V a d m) f = V (f a) d m'
  where
    --m' = M.mapKeys f m
    m' = M.fromDistinctAscList $ [(f b,w) | (b,w) <- M.toList m]

-- Blend the specified vertex from the two maps
blendVertices :: Ord v => v -> M.Map v (V v) -> M.Map v (V v) -> V v
blendVertices a ma mb = case (M.lookup a ma, M.lookup a mb) of 
  (Nothing, Nothing) -> NV
  (Nothing, Just v) -> v
  (Just v, Nothing) -> v
  -- NOTE: following v1,d1 and v2,d2 are identical in all possible cases
  (Just (V v1 d1 m1), Just (V _ _ m2)) -> V v1 d1 (M.union m1 m2)

has :: Ord v => G v -> v -> Bool
has (G n) v = M.member v n

ensureVertex :: Ord v => V v -> G v -> G v
ensureVertex NV g = g
ensureVertex (V a d m) (G n) = case M.lookup a n of
  Nothing -> G n' 
    where 
      v' = V a d m
      n' = M.insert a v' n
  Just _ -> G n

mapG :: (v -> v') -> G v -> G v'
mapG f (G n) = 
  let n' = M.fromDistinctAscList $ [(f a, v <**> f) | (a,v) <- M.toList n]
  in G n'

-- Create a unit graph with an initial vertex
pureG :: v -> G v
pureG v = G (M.singleton v (pureV v))    

-- Create a unit vertex
pureV :: v -> V v
pureV v = V v 1 M.empty

flatMapG :: G v -> (v -> G v') -> G v'
flatMapG _ _ = error "TAOTODO:"
-- flatMapG (G n m) f = G nf' mf' 
--   where n'  = [ | n <- M.toList n ]
--         nf' = ???
--         mf' = ???


