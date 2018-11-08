module Data.Connection.Base.Graph where

import qualified Data.Map as Map

data Ord a => Node a = NNode | Node { size :: a, id :: Int,  out :: Map.Map Int (Link a) }

data Ord a => Link a = NLink | Link { mag :: a, from :: Int, to :: Int }

-- Graph is a [Map] of identification number -> to [Node]
data Ord a => Gr a = Gr (Map.Map Int (Node a))

-- Graph size by number of nodes
gsize :: Ord a => Gr a -> Int
gsize g = error "TAOTODO:"

