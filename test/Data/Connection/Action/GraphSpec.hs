module Data.Connection.Action.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Graph(
  E, G, G(NullG), G(V), G(G),
  size, (<+>), (<:>), isin, newG, nodes)
import Data.Connection.Monad.GraphM
import Prelude hiding (min, max)
import Data.Maybe(fromJust)
import qualified Data.Map as Map
--import Data.Connection.Action.Graph()

main :: IO ()
main = hspec spec

g3 :: G String 
g3 = pure "A" <+> pure "B" <+> pure "C"

g3' :: G String 
g3' = ("B",1.0,"C") <:> (("A",5.0,"C") <:> (("A",2.5,"B") <:> g3))

spec :: Spec
spec = do
  describe "Basic Graph functions" $ do

    it "verifies the node" $ do
      (nodes g3) `shouldBe` ["A","B","C"]

    it "gets size of a graph" $ do 
      (size g3) `shouldBe` 3

    it "checks the existence of vertex" $ do
      ("A" `isin` g3) `shouldBe` True

    it "checks the existence of vertex (negative)" $ do
      ("x" `isin` g3) `shouldBe` False

    -- it "merge two graphs" $ do 
    --   error "TAOTODO:"

    -- it "get all paths from A to B" $ do 
    --   error "TAOTODO:"

