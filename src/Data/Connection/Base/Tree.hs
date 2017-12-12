module Data.Connection.Base.Tree where


data Ord a => Tree a = NTree |
              Tree { self  :: a
                   , left  :: Tree a 
                   , right :: Tree a 
                   }


sole :: Ord a => a -> Tree a 
sole n = Tree { self = n, left = NTree, right = NTree }

setLeft :: Ord a => Tree a -> Tree a -> Tree a 
setLeft t b = t { left = b }

setRight :: Ord a => Tree a -> Tree a -> Tree a 
setRight t b = t { right = b }

trimLeft :: Ord a => Tree a -> Tree a 
trimLeft t = t `setLeft` NTree

trimRight :: Ord a => Tree a -> Tree a 
trimRight t = t `setRight` NTree


