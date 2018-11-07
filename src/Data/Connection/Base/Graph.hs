module Data.Connection.Base.Graph where

data Ord a => G a = NG |
              G { self  :: a
                , nodes :: [G a]
                }

sole :: Ord a => a -> G a 
sole n = error "TAOTODO:"