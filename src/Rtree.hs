module Rtree
    ( Rtree
    , empty
    , insert
    ) where

import Coord(Coord(..))
import Point
import BoundingBox(BoundingBox(..), Contained(..))

import qualified BoundingBox as B

import Control.Arrow(second)
import Control.Monad.ST(runST)
import Data.Int(Int32, Int64)
import Data.List(minimumBy)
import Data.Vector((!), (//))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as VI

max_node_fill :: Int
max_node_fill = 9

min_node_fill :: Int
min_node_fill = 3

-- TODO use GADTs
data Cell = Inner { inner_bbox :: BoundingBox, inner_child :: Node }
          | Leaf { leaf_point :: Point, leaf_id :: Int32 }

data Node = Node { node_is_leaf :: Bool, node_cells :: V.Vector Cell }

instance Contained Cell where
    bbox Inner {inner_bbox, ..} = inner_bbox
    bbox Leaf {leaf_point, ..} = bbox leaf_point

instance Contained Node where
    bbox Node {node_cells, ..} = bbox node_cells

data Rtree  = Rtree { rtree_bbox :: BoundingBox, rtree_root :: Node }

instance Contained Rtree where
    bbox = rtree_bbox

mkInner :: Node -> Cell
mkInner c = Inner { inner_bbox = bbox c, inner_child = c }

margin :: BoundingBox -> Int64
margin b = i max_x - i min_x + i max_y - i min_y where
    i = fromIntegral . unwrapCoord . ($ b)

area :: BoundingBox -> Int64
area b = (i max_x - i min_x)*(i max_y - i min_y) where
    i = fromIntegral . unwrapCoord . ($ b)

splits :: V.Vector Cell -> [(Int, BoundingBox, BoundingBox)]
splits = undefined

chooseSplitAxis :: V.Vector Cell -> V.Vector Cell
chooseSplitAxis cells = let
    key f g c = let b = bbox c in
        (unwrapCoord $ f b, unwrapCoord $ g b)
    keyX = key min_x max_x
    keyY = key min_y max_y

    splitMargin (_, lhs, rhs) = margin lhs + margin rhs
    marginForSplits = sum . map splitMargin . splits

    sortBy key v = runST $ do
        vm <- V.thaw v
        VI.sortBy (compareOn key) vm
        V.freeze vm

    cells'x = sortBy keyX cells
    margin'x = marginForSplits cells'x

    cells'y = sortBy keyY cells
    margin'y = marginForSplits cells'y
    in if margin'x < margin'y then cells'x else cells'y

splitNode :: Node -> (Node, Node)
splitNode Node { node_is_leaf, node_cells } = let
    cells' = chooseSplitAxis node_cells
    (k, _, _) = minimumBy (compareOn f) (splits cells') where
        f (_, lhs, rhs) = (
            area (B.intersection lhs rhs), area (B.extend lhs rhs))
    mkNode cs = Node { node_is_leaf = node_is_leaf, node_cells = cs }
    (lhs, rhs) = V.splitAt k cells'
    in (mkNode lhs, mkNode rhs)

insertAt :: Node -> Cell -> (Node, Maybe Node)
insertAt Node { node_is_leaf, node_cells} c = let
    n' = Node { node_is_leaf = node_is_leaf
              , node_cells = V.snoc node_cells c }
    in if V.length node_cells <= max_node_fill
        then (n', Nothing)
        else second Just $ splitNode n'

compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn f x y = (f x) `compare` (f y)

chooseSubtree :: Node -> BoundingBox -> (Cell, Cell -> V.Vector Cell)
chooseSubtree Node { node_is_leaf, node_cells} box = let
    cellOverlap b index = V.ifoldr f 0 node_cells where
        f i c s
            | index == i = s
            | otherwise = s + area (B.intersection b (bbox c))

    metric = if node_is_leaf
        -- Choose the cell that needs least overlap enlargement.
        then \k b -> cellOverlap (B.extend box b) k - cellOverlap b k
        -- Choose the cell that needs least area enlargement.
        else \_ b -> area (B.extend box b) - area b

    cellMetric k = case node_cells ! k of
        Inner { inner_bbox, ..} -> metric k inner_bbox
        _ -> undefined

    index = minimumBy (compareOn cellMetric) [0..V.length node_cells]
    in (node_cells ! index, \x -> node_cells // [(index, x)])

insertInSubtree :: Node -> Cell -> (Node, Maybe Node)
insertInSubtree n c
    | node_is_leaf n = insertAt n c
    | otherwise = let (child, zipper) = chooseSubtree n (bbox c) in
        case child of
            Inner { inner_child, .. } -> let
                (new_child, new_child_sibling') = insertInSubtree inner_child c
                node' = Node {
                        node_is_leaf = False,
                        node_cells = zipper $ mkInner new_child }
                in case new_child_sibling' of
                    Nothing -> (node', Nothing)
                    Just new_child_sibling ->
                        insertAt node' (mkInner new_child_sibling)
            -- Cannot happen
            _ -> undefined

empty :: Rtree
empty = Rtree { rtree_bbox = B.empty
              , rtree_root = Node { node_is_leaf = True
                                  , node_cells = V.empty } }

insert :: Rtree -> Point -> Int32 -> Rtree
insert r p i = let
    (new_root, new_root_sibling') = insertInSubtree (rtree_root r) Leaf {
        leaf_point = p, leaf_id = i}
    root = case new_root_sibling' of
        Nothing -> new_root
        Just new_root_sibling -> Node {
            node_is_leaf = False,
            node_cells = V.fromList [
                mkInner new_root, mkInner new_root_sibling]}
    in Rtree { rtree_root = root, rtree_bbox = B.extend (bbox r) (bbox p) }
