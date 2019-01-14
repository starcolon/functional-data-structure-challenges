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
addOrReplaceG :: a -> G a -> [(a,G a)] -> [(a,G a)]
addOrReplaceG a v [] = [(a,v)]
addOrReplaceG a v (b:bs) = case snd b of 
  V a' _ _ -> case a' of 
    a -> ((a,v)):bs -- replace the old vertex
    _ -> b:(addOrReplaceG a v bs)
  G _ -> b:(addOrReplaceG a v bs)
  NullG -> b:(addOrReplaceG a v bs)

-- Add a new edge to the list of graph
addOrReplaceE :: a -> Double -> a -> [(a,G a)] -> [(a,G a)]
addOrReplaceE a w b [] = []
addOrReplaceE a w b (g:gs) = case fst g of 
  a -> (a,g'):gs
    where g' = setEdge a w b (snd g)
  _ -> g:(addOrReplaceE a w b gs)

unionList :: [(a,G a)] -> [(a,G a)] -> [(a,G a)]
unionList ns [] = ns
unionList (n:ns) ms = 
  let (a,n') = n 
    in unionList ns (addOrReplaceG a n' ms)

-- add or replace a vertex in a graph
-- the first argument is a new vertex to add 
setVertex :: G v -> G v -> G v
setVertex v g@(G m) = case v of 
  NullG -> g
  g'@(G m') -> G (unionList m m')
  v@(V a _ _) -> 
    let gs' = addOrReplaceG a v m
      in G gs'

addE :: v -> Double -> E v -> E v
addE a d [] = [(a,d)]
addE a d (b:bs) = case fst b of 
  a -> ((a,d)):bs -- Update the weight of an existing edge 
  _ -> b:(addE a d bs)

-- Set a link from a -> b
setEdge :: v -> Double -> v -> G v -> G v
setEdge a w b g = case g of 
  NullG -> g
  G m   -> G m'
    where m' = [(a',setEdge a w b g') | (a',g') <- m]
  v@(V u d e) -> case u of
    a -> V a d (addE a w e)
    _ -> g

-- Just a synnonym of 'setEdge' with different order of arguments
(~:~) :: G v -> (v,Double,v) -> G v
(~:~) g (a,w,b) = setEdge a w b g

-- Join two graphs
(<+>) :: G v -> G v -> G v
(<+>) NullG n = n
(<+>) n NullG = n
(<+>) v1@(V a _ _) v2@(V b _ _) = G [(a, v1), (b, v2)]
(<+>) v@(V a _ _) (G m) = G (addOrReplaceG a v m)
(<+>) (G m) v@(V _ _ _) = v <+> (G m)
(<+>) (G m1) (G m2) = G (unionList m1 m2)

getFromList :: v -> [(v,G v)] -> Maybe (G v)
getFromList v [] = Nothing
getFromList v (a:bs) = case fst a of 
  v -> case snd a of 
    NullG -> getFromList v bs
    g@(G m) -> case getFromList v m of
      Nothing -> getFromList v bs
      Just v' -> Just v'
    x@(V c _ _) -> case c of 
      v -> Just x
      _ -> Nothing
  _ -> getFromList v bs

get :: v -> G v -> Maybe (G v)
get v g = case g of 
  NullG       -> Nothing
  v@(V a _ _) -> case a of 
    v -> Just g
    _ -> Nothing
  g'@(G m)    -> getFromList v m

has :: v -> G v -> Bool
has v g = case get v g of
  Nothing -> False
  Just _  -> True

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

newG :: Double -> E v -> v -> G v
newG d e v = V v d e



