module Coord
    ( Coord(..)
    , toDouble
    , coordSucc
    ) where

import Data.Aeson(ToJSON(..))
import Data.Int(Int32)

multiplier :: Double
multiplier = fromIntegral (maxBound :: Int32) / 360

newtype Coord = Coord { unwrapCoord :: Int32 } deriving (Eq, Ord)

toDouble :: Coord -> Double
toDouble (Coord i) = (fromIntegral i) / multiplier

coordSucc :: Coord -> Coord
coordSucc (Coord i)
    | i /= maxBound = Coord (succ i)
    | otherwise = undefined

instance Show Coord where
    show = show . toDouble

instance ToJSON Coord where
    toJSON = toJSON . toDouble
