module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

data Node a = Node { value :: Double, key :: a, out :: M.Map a (Link a) }
data Link a = Link { mag :: Double, from :: a, to :: a }
data Route a = Route a a

-- Graph is a [Map] of keyentification number -> to [Node]
data Gr a = Gr (S.Set (Node a))

size :: Gr a -> Int 
size (Gr g) = S.size $ g

-- Create a new Graph from set of nodes
fromSet :: S.Set (Node a) -> Gr a 
fromSet s = Gr s

-- Create a unit node
unitNode :: a -> Node a 
unitNode v = Node {value = 1, key = v, out = M.empty}

-- Create a unit graph
unit :: a -> Gr a 
unit v = Gr (S.singleton $ unitNode v)

-- Apply new links to the graph.
-- The method is keyempotent.
link :: Gr a -> Link a -> Gr a 
link (Gr g) l = error "TAOTODO:"

-- Check whether a graph has the specifying node value
has :: Gr a -> a -> Bool 
has (Gr g) v = error "TAOTODO:"

applyNode :: (a -> b) -> Node a -> Node b
applyNode f v = v { key = (f v.key) }

-- Map from a graph of input type to a graph of output type
mapGr :: (a -> b) -> Gr a -> Gr b
mapGr f (Gr g) = 
  let fv = applyNode f
      ns = S.toList g
  in Gr (S.fromList $ map $ fv ns)

