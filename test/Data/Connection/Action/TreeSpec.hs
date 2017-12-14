module Data.Connection.Action.TreeSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len,
  setLeft, setRight, trimLeft, trimRight)
import Data.Connection.Action.Tree(addTo, toList)

main :: IO ()
main = hspec spec

tree7 :: Tree Double
tree7 = addTo 6.5 $ addTo 10.75 $ addTo 0.1 $ addTo 1.5 $ addTo 6.4 $ addTo 3.5 (sole 16.1)

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

    it "Should create a tree of length 7" $ do
      let l = len $ tree7
      l `shouldBe` 7

    it "Should convert tree to sorted list" $ do
      let ls = toList tree7
      ls `shouldBe` [0.1, 1.5, 3.5, 6.4, 6.5, 10.75, 16.1]

    it "Length of multi-level tree" $ do
      0 `shouldBe` 0 -- TAOTODO:
