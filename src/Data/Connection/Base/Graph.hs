module Data.Connection.Base.Graph where

import Foreign.Ptr

data Ord a => Node a = NNode | Node a

data Ord a => Link a = NLink | Link { ab :: Ptr (Node a), an :: Ptr (Node a), w :: a }

data Ord a => Gr a =
              Gr { nd :: [Node a]
                 , ln :: [Link a]
                 }

