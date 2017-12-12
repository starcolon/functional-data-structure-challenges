{-# LANGUAGE NamedFieldPuns #-}

module Data.Connection.Action.Tree where

import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len)


addTo :: Ord a => a -> Tree a -> Tree a 
addTo n t = case t of 
  NTree -> sole n
  Tree {self=s, left=l, right=r} -> 
    if s<n then
      t {left = n `addTo` l}
    else
      t {right = n `addTo` r}



  
