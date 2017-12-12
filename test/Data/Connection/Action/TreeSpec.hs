module Data.Connection.Action.TreeSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "foo" $ do
    it "bar" $ do
      1 `shouldBe` 1
