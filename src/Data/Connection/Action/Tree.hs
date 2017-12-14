{-# LANGUAGE NamedFieldPuns #-}

module Data.Connection.Action.Tree(
    addTo
  , removeFrom
  , toList
  , min
  , max
  , popMin
  , popMax
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



  
