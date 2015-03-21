module Coord
    ( Coord(..)
    , coordToDouble
    , coordSucc
    ) where

import Data.Int(Int32)

multiplier = fromIntegral (maxBound :: Int32) / 360

newtype Coord = Coord { unwrapCoord :: Int32 } deriving (Eq, Ord)

coordToDouble :: Coord -> Double
coordToDouble (Coord i) = (fromIntegral i) / multiplier

coordSucc :: Coord -> Coord
coordSucc (Coord i)
    | i /= maxBound = Coord (succ i)
    | otherwise = undefined

instance Show Coord where
    show = show . coordToDouble
