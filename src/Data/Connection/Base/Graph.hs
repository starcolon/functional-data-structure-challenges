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

-- Join two graphs
(<+>) :: G v -> G v -> G v
(<+>) (G va ma) (G vb mb) = error "TAOTODO:"

has :: Ord v => G v -> v -> Bool
has (G vs m) v = M.member v m

ensureVertex :: Ord v => V v -> G v -> G v
ensureVertex NV g = g
ensureVertex (V a d) (G vs m) = case M.lookup a m of
  Nothing -> G vs' m' 
    where 
      v'  = V a d
      vs' = v':vs
      m'  = M.insert a NE m
  Just es -> G vs m

similarEdge :: Ord v => E v -> E v -> Bool
similarEdge NE NE = True
similarEdge (E a b _) (E a' b' _) = a==a' && b==b'

ensureEdge :: Ord v => E v -> G v -> G v
ensureEdge NE g = g
ensureEdge (E a b d) (G vs m) = case M.lookup a m of
  Nothing -> ensureEdge (E a b d) g' 
    -- Insert a unit vertex if doesn't exist
    where g' = ensureVertex (V a 1) (G vs m)
  Just es -> 
    if any (\e -> e `isLinkTo` b) es
    -- Replace the old edge with the new one
    then let e' = E a b d
      in G vs (M.insert a es' m)
        where es' = [if similarEdge e e' then e' else e |e <- es]
    -- Add a new edge
    else G vs (M.fromDistinctAscList $ (E a b d):es)

isLinkTo :: E v -> v -> Bool
isLinkTo NE _ = False
isLinkTo (E a b _) a' = a == a'

mapG :: (v -> v') -> G v -> G v'
mapG f (G vs m) = G [fv f n | n <- vs] (M.fromDistinctAscList $ [(f a, fe f b) | (a,b) <- M.toList m])

-- Create a unit graph with an initial vertex
pureG :: v -> G v
pureG v = G vs m
  where 
    vs = [V v 1]
    m  = M.singleton v NE

flatMapG :: G v -> (v -> G v') -> G v'
flatMapG (G vs m) f = error "TAOTODO:"


