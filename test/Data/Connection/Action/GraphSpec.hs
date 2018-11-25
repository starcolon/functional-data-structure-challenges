module Data.Connection.Action.GraphSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck()
import Data.Maybe()
import Data.Connection.Base.Graph(Gr,Route,Node,Link)
import Prelude hiding (min, max)
import Data.Maybe(fromJust)
import qualified Data.Map as Map
--import Data.Connection.Action.Graph()

main :: IO ()
main = hspec spec

g5 :: Gr Double 
g5 = Gr (Map.Map ) -- TAOTODO:

spec :: Spec
spec = do
  describe "Basic Graph functions" $ do

    it "get size of a graph" $ do 
      error "TAOTODO:"

    it "get all paths from A to B" $ do 
      error "TAOTODO:"

