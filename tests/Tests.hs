module Main where

import Coord
import Point
import BoundingBox(BoundingBox(..), Contained(..))

import qualified BoundingBox as B
import qualified Rtree as R

import Data.List(foldl', sort)
import Data.Vector((!))
import System.Random(mkStdGen, Random(..))
import Test.Hspec

import qualified Data.Vector as V

point1 :: Point
point1 = Point (Coord 1) (Coord 1)

point2 :: Point
point2 = Point (Coord 2) (Coord 2)

emptyRtree :: SpecWith ()
emptyRtree = it "empty one does not have points" $ let
    found = R.lookup BoundingBox {
        min_x = Coord(1),
        min_y = Coord(1),
        max_x = Coord(2),
        max_y = Coord(2)} R.empty
    in found `shouldBe` []

oneElement :: SpecWith ()
oneElement = it "with one element has this element" $ do
    let r = R.insert point1 1 R.empty
    R.lookup (bbox point1) r `shouldBe` [1]
    R.lookup (bbox point2) r `shouldBe` []

randomPoints :: Int -> Int -> [Point]
randomPoints seed n = let
    stream = randomRs (-1000, 1000) (mkStdGen seed)
    xs = take n stream
    ys = take n $ drop n stream
    mkPoint x y = Point (Coord x) (Coord y)
    in zipWith mkPoint xs ys

mkRandomTest :: Int -> Int -> IO ()
mkRandomTest seed n = let
    ps = randomPoints seed n
    ids = [0..fromIntegral n - 1]
    r = foldl' (\r (p, i) -> R.insert p i r) R.empty $ zip ps ids
    psv = V.fromList ps
    in sequence_ $ do
        i <- [0..n - 1]
        j <- [i + 1..n - 1]
        let box = B.extend (bbox (psv ! i)) (bbox (psv ! j))
        let ans = filter (\i -> B.contains box (psv ! (fromIntegral i))) ids
        return $ sort (R.lookup box r) `shouldBe` ans

randomSmall :: SpecWith ()
randomSmall = it "small random one works" (mkRandomTest 1 4)

randomMid :: SpecWith ()
randomMid = it "mid random one works" (mkRandomTest 1 10)

randomBig :: SpecWith ()
randomBig = it "big random one works" (mkRandomTest 1 100)

main :: IO ()
main = hspec $ do
    describe "Rtree" $ do
        emptyRtree
        oneElement
        randomSmall
        randomMid
        randomBig
