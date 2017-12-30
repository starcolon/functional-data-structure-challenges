module Data.Connection.Action.TreeSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Tree(
  Tree, Tree(NTree), Tree(Tree), 
  sole, self, left, right, len,
  setLeft, setRight, trimLeft, trimRight)
import Prelude hiding (min, max)
import Data.Maybe(fromJust)
import Data.Connection.Action.Tree(
  addTo, toList, min, max,
  popMin, popMax, removeFrom,
  (+:+), (-:-),
  has, depth, rebalance, when,
  fromList, intersect, mapTree)

main :: IO ()
main = hspec spec

tree3 :: Tree Double 
tree3 = addTo 7.45 $ addTo 2.25 (sole 0.0)

tree7 :: Tree Double
tree7 = addTo 6.5 $ addTo 10.75 $ addTo 0.1 $ addTo 1.5 $ addTo 6.4 $ addTo 3.5 (sole 16.1)

treeTuples :: Tree (Int, Int)
treeTuples = addTo (-1,5) $ addTo (-1,0) $ addTo (1,12 )$ addTo (0,5) (sole (0,0))

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
      ll `shouldBe` 3

    it "length of sole tree is 1" $ do
      let l = len $ sole 7
      l `shouldBe` 1

    it "should create a tree of length 7" $ do
      let l = len $ tree7
      l `shouldBe` 7

    it "should find minima" $ do
      let m = min $ tree7
          e = Just 0.1
      m `shouldBe` e

    it "should find maxima" $ do
      let m = max $ tree7
          e = Just 16.1
      m `shouldBe` e

    it "should convert tree to sorted list" $ do
      let ls = toList tree7
      ls `shouldBe` [0.1, 1.5, 3.5, 6.4, 6.5, 10.75, 16.1]

    it "should pop min" $ do
      let (m,t) = popMin tree7
          m'    = fromJust m
          m''   = fromJust $ min t
      (m',m'') `shouldBe` (0.1, 1.5)

    it "should pop max" $ do
      let (m,t) = popMax tree7
          m'    = fromJust m
          m''   = fromJust $ max t
      (m',m'') `shouldBe` (16.1, 10.75)

    it "should remove a middle node" $ do
      let t = 6.4 `removeFrom` tree7
          l = toList t
      l `shouldBe` [0.1, 1.5, 3.5, 6.5, 10.75, 16.1]

    it "should remove multiple nodes" $ do
      let t = removeFrom 1.5 $ removeFrom 10.75 $ removeFrom 6.5 $ removeFrom 16.1 tree7
          l = toList t
      l `shouldBe` [0.1, 3.5, 6.4]

    it "should remain unchanged when remove a node which doesn't exist" $ do
      let t = removeFrom 3.4 tree7
          l = toList t
          l' = toList tree7
      l `shouldBe` l'

    it "should check if element exists" $ do
      let b = tree7 `has` 10.75
      b `shouldBe` True

    it "should check if element exists (2)" $ do
      let b = tree7 `has` 1.5
      b `shouldBe` True

    it "should check if element doesn't exist" $ do
      let b = tree7 `has` 16.75
      b `shouldBe` False

    it "should check if element doesn't exist (2)" $ do
      let b = tree7 `has` (-1.5)
      b `shouldBe` False

    it "should merge a tree with empty tree" $ do
      let t = tree7 +:+ NTree
          l = toList tree7
          l' = toList t
      l `shouldBe` l'

    it "should merge two trees" $ do
      let l = toList $ tree7 +:+ tree3 
      l `shouldBe` [0.0, 0.1, 1.5, 2.25, 3.5, 6.4, 6.5, 7.45, 10.75, 16.1]

    it "should merge large trees" $ do
      let l = toList $ tree7 +:+ tree7 +:+ tree7 
      l `shouldBe` [0.1, 0.1, 0.1, 1.5, 1.5, 1.5, 3.5, 3.5, 3.5, 6.4, 6.4, 6.4, 6.5, 6.5, 6.5, 10.75, 10.75, 10.75, 16.1, 16.1, 16.1]

    it "should remove trees" $ do
      let t = addTo 0.1 $ addTo 0.0 $ addTo 10.75 $ sole 16.1
          l = toList $ tree7 -:- t
      l `shouldBe` [1.5, 3.5, 6.4, 6.5]

    it "should find the depth of a tree" $ do
      let t0 = addTo 1 $ addTo 3 $ sole 2
          t1 = addTo 9 $ addTo 7 $ addTo 6 t0
          d = map depth [t0,t1]
      d `shouldBe` [2, 5]

    it "sole tree has depth of 1" $ do
      let d = depth $ sole 4
      d `shouldBe` 1

    it "should rebalance an unbalanced tree" $ do
      let t0 = addTo 1 $ addTo 3 $ sole 2
          t1 = addTo 15 $ addTo 9 $ addTo 7 $ addTo 6 t0
          t2 = rebalance t1
          dl = depth $ left t2
          dr = depth $ right t2
          d = abs $ dl - dr
      d `shouldSatisfy` (<=1)

    it "should create a tree of huge list then balance" $ do 
      let t0 = fromList [1..444]
          t1 = rebalance t0
          dl = depth $ left t1
          dr = depth $ right t1
          absdiff = abs $ dl - dr
      absdiff `shouldSatisfy` (<=1)

    it "should filter a tree by conditional function" $ do 
      let l = toList $ when tree7 (>3.5) 
      l `shouldBe` [6.4, 6.5, 10.75, 16.1]

    it "should filter a tree by conditional function (2)" $ do
      let l = toList $ when treeTuples (\x -> snd x == 0)
      l `shouldBe` [(-1,0), (0,0)]

    it "should find intersection with empty tree" $ do
      let l = toList $ NTree `intersect` tree7
      l `shouldBe` []

    it "should find intersection between two trees" $ do
      let t1 = addTo "Z" $ addTo "C" $ addTo "B" $ sole "A"
          t2 = addTo "Z" $ addTo "V" $ addTo "A" $ sole "K"
          l = toList $ t1 `intersect` t2
      l `shouldBe` ["A","Z"]

    it "should map tree" $ do 
      let l = toList $ mapTree tree3 (*10)
      l `shouldBe` [0, 22.5, 74.5]
