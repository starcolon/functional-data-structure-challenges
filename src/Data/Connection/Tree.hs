module Data.Connection.Tree where


-- A branch could be any of following:
--  • Leaf node, no more extended branch from this point
--  • Nothing at all, no leaf, no branch
--  • A child branch
data Branch a = Leaf a | NoBranch | Branch a


data Tree a = Tree { self  :: a
                   , left  :: Branch a
                   , right :: Branch a 
                   }


sole :: a -> Tree a 
sole n = Tree { self = n, left = NoBranch, right = NoBranch }

setLeft :: Tree a -> Branch a -> Tree a 
setLeft t b = t { left = b }

setRight :: Tree a -> Branch a -> Tree a 
setRight t b = t { right = b }

trimLeft :: Tree a -> Tree a 
trimLeft t = t `setLeft` NoBranch

trimRight :: Tree a -> Tree a 
trimRight t = t `setRight` NoBranch

