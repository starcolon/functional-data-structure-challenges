module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

data Eq a => Node a = Node { value :: Double, id :: a, out :: M.Map a (Link a) }
data Eq a => Link a = Link { mag :: Double, from :: a, to :: a }
data Route = Route Int Int

-- Graph is a [Map] of identification number -> to [Node]
data Eq a => Gr a = Gr (S.Set (Node a))

size :: Eq a => Gr a -> Int 
size (Gr g) = S.size $ g

new :: Eq a => S.Set (Node a) -> Gr a 
new s = error "TAOTODO:"