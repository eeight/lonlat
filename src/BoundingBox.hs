module BoundingBox
    ( BoundingBox(..)
    , Contained(..)
    , intersects
    , intersection
    , contains
    , isEmpty
    , empty
    , extend
    , extend'
    ) where

import Coord
import Point

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import qualified Data.Vector as V

data BoundingBox = BoundingBox
    { min_x :: Coord
    , min_y :: Coord
    , max_x :: Coord
    , max_y :: Coord
    } deriving (Eq, Generic)

instance ToJSON BoundingBox

intersects :: BoundingBox -> BoundingBox -> Bool
intersects b1 b2 = minmax max_x min_x && minmax max_y min_y
    where minmax p1 p2 = min (p1 b1) (p1 b2) > max (p2 b1) (p2 b2)

intersection :: BoundingBox -> BoundingBox -> BoundingBox
intersection b1 b2 = let
    minx = max (min_x b1) (min_x b2)
    miny = max (min_y b1) (min_y b2)
    maxx = min (max_x b1) (max_x b2)
    maxy = min (max_y b1) (max_y b2) in
    BoundingBox {
        min_x = minx,
        min_y = miny,
        max_x = max maxx minx,
        max_y = max maxy miny
    }

contains :: BoundingBox -> Point -> Bool
contains b (Point x y) =
    min_x b <= x && x < max_x b && min_y b <= y && y < max_y b

isEmpty :: BoundingBox -> Bool
isEmpty b = min_x b == max_x b || min_y b == max_y b

empty :: BoundingBox
empty = BoundingBox { min_x = z, min_y = z, max_x = z, max_y = z}
    where z = Coord(0)

extend :: BoundingBox -> BoundingBox -> BoundingBox
extend b1 b2
    | isEmpty b1 = b2
    | isEmpty b2 = b1
    | otherwise = BoundingBox {
        min_x = min (min_x b1) (min_x b2),
        min_y = min (min_y b1) (min_y b2),
        max_x = max (max_x b1) (max_x b2),
        max_y = max (max_y b1) (max_y b2)
    }

class Contained a where
    bbox :: a -> BoundingBox

instance Contained BoundingBox where
    bbox = id

instance Contained Point where
    bbox (Point x y) = BoundingBox {
        min_x = x, min_y = y, max_x = coordSucc x, max_y = coordSucc y }

extend' :: (Contained a, Contained b) => a -> b -> BoundingBox
extend' a b = extend (bbox a) (bbox b)

instance Contained t => Contained [t] where
    bbox = foldr extend' empty

instance Contained t => Contained (V.Vector t) where
    bbox = V.foldr extend' empty
