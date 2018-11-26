module Data.Connection.Base.Graph where

import qualified Data.Map as M
import qualified Data.Set as S

-- Graph of vertex type [v]
data G v = G (S.Set (V v)) (M.Map v (E v))
-- Vertex of type [v]
data V v = NV | V v Double
-- Edge of type [v]
data E v = NE | E v v Double

size :: G v -> Int
size (G set _) = S.size $ set

mapG :: (v -> v') -> G v -> G v'
mapG f g = error "TAOTODO:"

pureG :: v -> G v
pureG v = error "TAOTODO:"