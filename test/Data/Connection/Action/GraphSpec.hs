module Data.Connection.Action.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Graph(
  G,G(G),
  V,V(V),V(NV),
  E,E(E),E(NE),
  size)
import Prelude hiding (min, max)
import Data.Maybe(fromJust)
import qualified Data.Map as Map
--import Data.Connection.Action.Graph()

main :: IO ()
main = hspec spec

g3 :: G String 
g3 = G [(V "a" 1), (V "b" 1), (V "c" 15)] Map.empty

spec :: Spec
spec = do
  describe "Basic Graph functions" $ do

    it "get size of a graph" $ do 
      (size g3) `shouldBe` 3

    -- it "check the existence of vertex" $ do
    --   error "TAOTODO:"

    -- it "merge two graphs" $ do 
    --   error "TAOTODO:"

    -- it "get all paths from A to B" $ do 
    --   error "TAOTODO:"

