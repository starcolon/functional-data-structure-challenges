module Data.Connection.Action.TreeSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len,
  setLeft, setRight, trimLeft, trimRight)
import Data.Connection.Action.Tree(addTo)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic Tree functions" $ do

    it "sole tree does not have branches" $ do
      let t  = sole 10
          lr = map len [left t, right t]
      lr `shouldBe` [0, 0]

    it "should add branches" $ do
      let t  = sole 10
          t1 = t `setLeft` (sole 1)
          t2 = t1 `setRight` (sole 15)
          ll = len t2
      ll `shouldBe` 3 -- TAOTODO: Also check for values

    it "Length of sole tree is 1" $ do
      let l = len $ sole 7
      l `shouldBe` 1

    it "Length of multi-level tree" $ do
      0 `shouldBe` 0 -- TAOTODO:
