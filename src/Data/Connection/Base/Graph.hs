module Data.Connection.Base.Graph where

import qualified Data.Map as Map

data Ord a => Node a = NNode | Node { size :: a, id :: Int,  out :: Map.Map Int (Link a) }

data Ord a => Link a = NLink | Link { mag :: a, from :: Int, to :: Int }

data Route = Route Int Int

-- Graph is a [Map] of identification number -> to [Node]
data Ord a => Gr a = Gr (Map.Map Int (Node a))

-- Graph size by number of nodes
gsize :: Ord a => Gr a -> Int
gsize g = error "TAOTODO:"

-- Add a new node
(++) :: Ord a => Gr a -> Node a -> Gr a 
(++) g n = error "TAOTODO:"

-- Add a new link 
(**) :: Ord a => Gr a -> Link a -> Gr a
(**) g l = error "TAOTODO:"

-- Query links between two nodes
paths :: Ord a => Gr a -> Route -> Maybe (Link a)
paths g (Route i j) = error "TAOTODO:"

-- Find all possible routes from a node to another
traverse :: Ord a => Gr a -> Route -> [[Link a]]
traverse g (Route i j) = error "TAOTODO:"

-- Find the shortest path between two nodes
shortest :: Ord a => Gr a -> Route -> [Link a]
shortest g (Route i j) = error "TAOTODO:"