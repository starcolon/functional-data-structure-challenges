{-# LANGUAGE NamedFieldPuns #-}

module Data.Connection.Action.Tree(
    addTo
  , toList
  , min
  , max
  ) where

import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len)
import Prelude hiding(min, max)


addTo :: Ord a => a -> Tree a -> Tree a 
addTo n t = case t of 
  NTree -> sole n
  Tree {self=s, left=l, right=r} -> 
    if s>n then
      t {left = n `addTo` l}
    else
      t {right = n `addTo` r}


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



  
