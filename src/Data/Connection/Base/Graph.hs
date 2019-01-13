module Data.Connection.Base.Graph where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

-- Leave nodes of graph can be another graph or a vertex
type E v = [(v, Double)]
data G v 
  = NullG 
  | V v Double (E v)
  | G [(v,G v)] deriving (Show,Eq)

size :: Ord v => G v -> Int
size g = case g of
  NullG   -> 0
  V _ _ _ -> 1
  G m     -> Prelude.foldr (+) 0 [size b | (a,b) <- m]

coreMaybe :: Ord v => G v -> Maybe (v, Double, E v)
coreMaybe NullG = Nothing
coreMaybe (V v d e) = Just ((v,d,e))
coreMaybe (G m) = 
  let (v,g) = head m
    in case g of 
      NullG   -> Nothing 
      V u d e -> Just ((u,d,e))
      G q     -> coreMaybe $ G q

iter :: G v -> [(v, Double, E v)]
iter NullG = []
iter (V v d e) = [(v, d, e)]
iter (G m) = Prelude.foldr (++) [] [iter g | (v,g) <- m]

-- Fold multiple graphs into one
foldG :: [G v] -> G v
foldG [] = error "Unable to fold empty list of graphs"
foldG (n:[]) = n
foldG (a:b:bs) = foldG (h':bs)
  where h' = (a <+> b)

-- Given a list of pairs of (a,G a)
-- add a new (a, G a) to it
-- or update the existing one if already does
addOrReplace :: a -> G a -> [(a,G a)] -> [(a,G a)]
addOrReplace a v [] = [(a,v)]
addOrReplace a v (b:bs) = case snd b of 
  V a' _ _ -> case a' of 
    a -> ((a,v)):bs -- replace the old vertex
    _ -> b:(addOrReplace a v bs)
  G _ -> b:(addOrReplace a v bs)
  NullG -> b:(addOrReplace a v bs)

unionList :: [(a,G a)] -> [(a,G a)] -> [(a,G a)]
unionList ns [] = ns
unionList (n:ns) ms = 
  let (a,n') = n 
    in unionList ns (addOrReplace a n' ms)

-- add or replace a vertex in a graph
-- the first argument is a new vertex to add 
setVertex :: G v -> G v -> G v
setVertex v g@(G m) = case v of 
  NullG -> g
  g'@(G m') -> G (unionList m m')
  v@(V a _ _) -> 
    let gs' = addOrReplace a v m
      in G gs'

-- Join two graphs
(<+>) :: G v -> G v -> G v
(<+>) NullG n = n
(<+>) n NullG = n
(<+>) v1@(V a _ _) v2@(V b _ _) = G [(a, v1), (b, v2)]
(<+>) v@(V a _ _) (G m) = G (addOrReplace a v m)
(<+>) (G m) v@(V _ _ _) = v <+> (G m)
(<+>) (G m1) (G m2) = G (unionList m1 m2)

has :: Ord v => G v -> v -> Bool
has (G n) v = any pred gs
  where
    pred = \(v',d,e) -> v' == v
    gs = iter $ G n

applyE :: (v -> v') -> E v -> E v'
applyE f m = [(f v,d) | (v,d) <- m]

mapG :: (v -> v') -> G v -> G v'
mapG f NullG = NullG
mapG f (V v d e) = V (f v) d (applyE f e)
mapG f (G m) = foldG gs
  where
    gs = [V (f v) d (applyE f e) | (v,d,e) <- iter $ G m]

pureG :: (v,Double,E v) -> G v
pureG (v,d,e) = V v d e

-- Create a unit graph with an initial vertex
unitG :: v -> G v
unitG v = V v 1 []



