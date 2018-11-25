module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

data Node a = Node { value :: Double, id :: a, out :: M.Map a (Link a) }
data Link a = Link { mag :: Double, from :: a, to :: a }
data Route a = Route a a

-- Graph is a [Map] of identification number -> to [Node]
data Gr a = Gr (S.Set (Node a))

size :: Gr a -> Int 
size (Gr g) = S.size $ g

-- Create a new Graph from set of nodes
fromSet :: S.Set (Node a) -> Gr a 
fromSet s = Gr s

-- Apply new links to the graph.
-- The method is idempotent.
link :: Gr a -> Link a -> Gr a 
link (Gr g) l = error "TAOTODO:"

-- Check whether a graph has the specifying node value
has :: Gr a -> a -> Bool 
has (Gr g) v = error "TAOTODO:"

-- Map from a graph of input type to a graph of output type
mapGr :: (a -> b) -> Gr a -> Gr b
mapGr f (Gr g) = error "TAOTODO:"

