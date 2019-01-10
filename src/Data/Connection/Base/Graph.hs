module Data.Connection.Base.Graph where

import qualified Data.Set as S
import qualified Data.Map as M

-- Graph is a composition of nodes or graphs itself
data E v = M.Map v Double deriving (Show,Eq)
data G v = NullG | 
          V v Double (E v) | 
          G (M.Map v (G v)) deriving (Show,Eq)

size :: Ord v => G v -> Int
size (G n) = case n of
  NullG   -> 0
  V _ _ _ -> 1
  G m     -> Prelude.foldr $ (+) 0 [size b | (a,b) <- M.toList m]

coreMaybe :: Ord v => G v -> Maybe (v, Double, E v)
coreMaybe NullG = Nothing
coreMaybe (V v d e) = Just ((v,d,e))
coreMaybe (G m) = 
  let (v,g) = head $ M.toList m
    in case g of 
      NullG   -> Nothing 
      V u d e -> Just ((u,d,e))
      G q     -> coreMaybe $ G q

iter :: G v -> [(v, Double, E v)]
iter NullG = []
iter (V v d e) = [(v, d, e)]
iter (G m) = Prelude.foldr $ (++) [] [iter g | (v,g) <- M.toList m]

-- Fold multiple graphs into one
foldG :: Ord v => [G v] -> G v
foldG [] = error "Unable to fold empty list of graphs"
foldG (n:[]) = n
foldG (a:b:bs) = foldG $ (a <+> b):bs

-- Join two graphs
(<+>) :: Ord v => G v -> G v -> G v
(<+>) NullG n = n
(<+>) n NullG = n
(<+>) (V v1 d1 e1) (V v2 d2 e2) = G (M.fromDistinctAscList [(v1, V v1 d1 e1), (v2, V v2 d2 e2)])
(<+>) (V v d e) (G m) = G (M.insert $ v (V v d e) m)
(<+>) (G m) (V v d e) = (V v d e) <+> (G m)
(<+>) (G m1) (G m2) = G (M.union m1 m2)

has :: Ord v => G v -> v -> Bool
has (G n) v = any pred gs
  where
    pred = \(v',d,e) -> v' == v
    gs = iter $ G n

applyE :: (v -> v') -> E v -> E v'
applyE f m = M.fromDistinctAscList [(f v,d) | (v,d) <- M.toList m]

mapG :: (v -> v') -> G v -> G v'
mapG f NullG = NullG
mapG f (V v d e) = V (f v) d e
mapG f (G m) = foldG gs
  where
    gs = [V (f v) d (applyE f e) | (v,d,e) <- iter $ G m]

pureG :: (v,Double,E v) -> G v
pureG (v,d,e) = V v d e

-- Create a unit graph with an initial vertex
unitG :: v -> G v
unitG v = V v 1 M.empty

flatMapG :: G v -> (v -> G v') -> G v'
flatMapG g f = foldG [(f v,d,applyE f e) | (v,d,e) <- iter $ g]

