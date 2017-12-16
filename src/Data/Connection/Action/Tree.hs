{-# LANGUAGE NamedFieldPuns #-}

module Data.Connection.Action.Tree(
    addTo
  , removeFrom
  , toList
  , depth
  , has
  , min
  , max
  , popMin
  , popMax
  , rebalance
  , (+:+)
  , (-:-)
  ) where

import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len)
import Prelude hiding(min, max)
import Data.Maybe(fromJust)


addTo :: Ord a => a -> Tree a -> Tree a 
addTo n t = case t of 
  NTree -> sole n
  Tree {self=s, left=l, right=r} -> 
    if s>n then
      t {left = n `addTo` l}
    else
      t {right = n `addTo` r}

addListTo :: Ord a => [a] -> Tree a -> Tree a 
addListTo ns t = case ns of 
  [] -> t
  x:xs -> xs `addListTo` (x `addTo` t)

removeFrom :: Ord a => a -> Tree a -> Tree a
removeFrom n t = case t of 
  NTree -> NTree
  Tree {self=s, left=l, right=r} ->
    if s>n then
      t {left = n `removeFrom` l}
    else if s==n then
      removeSelf t
    else 
      t {right = n `removeFrom` r}

removeSelf :: Ord a => Tree a -> Tree a 
removeSelf t = case t of 
  NTree -> NTree
  Tree {self=s, left=NTree, right=NTree} -> NTree
  Tree {self=s, left=l, right=NTree} -> l
  Tree {self=s, left=NTree, right=r} -> r
  Tree {self=s, left=l, right=r} -> 
    -- Min of the left children will become new self
    let (m,l') = popMin $ l
    in Tree {self=fromJust m, left=l', right=r}

popMin :: Ord a => Tree a -> (Maybe a, Tree a)
popMin t = let m = min t
  in case m of 
    Nothing -> (Nothing, t)
    Just m' -> (Just m', m' `removeFrom` t)

popMax :: Ord a => Tree a -> (Maybe a, Tree a)
popMax t = let m = max t
  in case m of 
    Nothing -> (Nothing, t)
    Just m' -> (Just m', m' `removeFrom` t)


toList :: Ord a => Tree a -> [a]
toList t = case t of 
  NTree -> []
  Tree {self=s, left=l, right=r} ->
    let ll = toList l
        lr = toList r 
      in ll ++ [s] ++ lr

terminal :: Ord a => (Tree a -> Tree a) -> (Tree a -> Maybe a) -> Tree a -> Maybe a
terminal f g t = case t of 
  NTree -> Nothing
  otherwise -> case f t of 
    NTree -> Just (self t)
    otherwise -> g $ f t

max :: Ord a => Tree a -> Maybe a 
max t = terminal right max t

min :: Ord a => Tree a -> Maybe a
min t = terminal left min t

has :: Ord a => Tree a -> a -> Bool
has t n = case t of 
  NTree -> False
  Tree {self=s, left=l, right=r}
    | s==n      -> True
    | s>n       -> l `has` n
    | otherwise -> r `has` n

rebalance :: Ord a => Tree a -> Tree a 
rebalance t = 
  let dl = depth $ left t
      dr = depth $ right t
    in if abs(dl - dr)<=1 then t
    else 
      let l = toList t
          m = l !! (div (length l -1) 2)
          l' = filter (/=m) l
          t0 = sole m
      in addListTo l' t0

depth :: Ord a => Tree a -> Int 
depth t = case t of 
  NTree -> 0
  Tree {self=s, left=l, right=r} ->
    let [d1,d2] = map depth [l,r]
      in 1 + (if d1>d2 then d1 else d2)

-- "with"
(+:+) :: Ord a => Tree a -> Tree a -> Tree a 
(+:+) t1 t2 = case (t1,t2) of
  (NTree,_)   -> t2
  (_,NTree)   -> t1
  otherwise   -> 
    let 
        [s1,s2] = map self [t1,t2]
        [l1,l2] = map left [t1,t2]
        [r1,r2] = map right [t1,t2]
    in if s1 > s2
      then t1 {left = (addTo s2 l1) +:+ l2} +:+ r2
      else t1 {right = (addTo s2 r1)} +:+ l2 +:+ r2


-- "without"
(-:-) :: Ord a => Tree a -> Tree a -> Tree a
(-:-) t1 t2 = case (t1,t2) of 
  (NTree,_) -> t2
  (_,NTree) -> t1
  otherwise -> ((removeFrom (self t2) t1) -:- (left t2)) -:- (right t2)
          



  
