module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

data Eq a => Node a = Node { value :: Double, id :: a, out :: M.Map a (Link a) }
data Eq a => Link a = Link { mag :: Double, from :: a, to :: a }
data Route a = Route a a

-- Graph is a [Map] of identification number -> to [Node]
data Eq a => Gr a = Gr (S.Set (Node a))

size :: Eq a => Gr a -> Int 
size (Gr g) = S.size $ g

-- Create a new Graph from set of nodes
fromSet :: Eq a => S.Set (Node a) -> Gr a 
fromSet s = Gr s

-- Apply new links to the graph.
-- The method is idempotent.
link :: Eq a => Gr a -> Link a -> Gr a 
link (Gr g) l = error "TAOTODO:"

-- Check whether a graph has the specifying node value
has :: Eq a => Gr a -> a -> Bool 
has (Gr g) v = error "TAOTODO:"


