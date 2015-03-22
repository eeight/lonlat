module Main where

import BoundingBox
import Coord
import Point
import qualified Rtree as R

import Test.Hspec

point1 :: Point
point1 = Point (Coord 1) (Coord 1)

point2 :: Point
point2 = Point (Coord 2) (Coord 2)

main :: IO ()
main = hspec $ do
    describe "Rtree tests" $ do
        it "Empty Rtree is empty" $ do
            let found = R.lookup BoundingBox {
                min_x = Coord(1),
                min_y = Coord(1),
                max_x = Coord(2),
                max_y = Coord(2)} R.empty
            found `shouldBe` []
