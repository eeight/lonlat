module Point(Point(..)) where

import Coord

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

data Point = Point Coord Coord deriving Generic

instance ToJSON Point
