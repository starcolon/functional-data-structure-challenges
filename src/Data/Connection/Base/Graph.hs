module Data.Connection.Base.Graph where

import qualified Data.Map as Map

data Eq a => Node a = Node { size :: Double, id :: a, out :: Map.Map a (Link a) }
data Eq a => Link a = Link { mag :: Double, from :: a, to :: a }
data Route = Route Int Int

-- Graph is a [Map] of identification number -> to [Node]
data Eq a => Gr a = Gr (Map.Map Int (Node a))
