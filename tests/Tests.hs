module Main where

import Coord
import Point
import BoundingBox(BoundingBox(..), Contained(..))

import qualified BoundingBox as B
import qualified Rtree as R

import Data.List(foldl', sort)
import Data.Vector((!))
import Data.Int(Int32)
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
    (xs, stream') = splitAt n stream
    ys = take n stream'
    mkPoint x y = Point (Coord x) (Coord y)
    in zipWith mkPoint xs ys

mkTest :: [Point] -> [Int32] -> R.Rtree -> IO ()
mkTest ps ids r = sequence_ $ do
        let n = length ps
        let psv = V.fromList ps
        let idsv = V.fromList ids
        i <- [0..n - 1]
        j <- [i + 1..n - 1]
        let box = B.extend (bbox (psv ! i)) (bbox (psv ! j))
        let ans = [idsv ! k | k <- [0..n - 1], B.contains box (psv ! k)]
        return $ sort (R.lookup box r) `shouldBe` ans

mkRandomTest :: Int -> Int -> IO ()
mkRandomTest seed n = let
    ps = randomPoints seed n
    ids = [0..fromIntegral n - 1]
    r = foldl' (\r (p, i) -> R.insert p i r) R.empty $ zip ps ids

    n2 = n `div` 2
    (psToDelete, ps') = splitAt n2 ps
    (idsToDelete, ids') = splitAt n2 ids
    r' = foldl' (\r (p, i) -> R.delete p i r) r $ zip psToDelete idsToDelete

    in mkTest ps ids r >> mkTest ps' ids' r'

randomSmall :: SpecWith ()
randomSmall = it "small random one works" (mkRandomTest 1 4)

randomMid :: SpecWith ()
randomMid = it "mid random one works" (mkRandomTest 1 10)

randomBig :: SpecWith ()
randomBig = it "big random one works" (mkRandomTest 1 100)

randomHuge :: SpecWith ()
randomHuge = it "big random one works" (mkRandomTest 1 1000)

main :: IO ()
main = hspec $ do
    describe "Rtree" $ do
        emptyRtree
        oneElement
        randomSmall
        randomMid
        randomBig
        --randomHuge
