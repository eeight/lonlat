module Rtree
    ( Rtree
    , empty
    , insert
    , lookup
    ) where

import Coord(Coord(..))
import Point
import BoundingBox(BoundingBox(..), Contained(..))

import qualified BoundingBox as B

import Prelude hiding (lookup)
import Control.Monad.ST(runST)
import Data.Aeson(ToJSON(..), (.=), object)
import Data.Int(Int32, Int64)
import Data.List(minimumBy)
import Data.Vector((!), (//))

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

max_node_fill :: Int
max_node_fill = 9

min_node_fill :: Int
min_node_fill = 3

data Position = Leaf | Inner

data Cell p where
    LeafCell :: Point -> Int32 -> Cell Leaf
    InnerCell :: forall p'. BoundingBox -> Node p' -> Cell Inner

data Node p where
    LeafNode :: V.Vector (Cell Leaf) -> Node Leaf
    InnerNode :: V.Vector (Cell Inner) -> Node Inner

instance ToJSON (Cell a) where
    toJSON (LeafCell p i) = object ["point" .= p, "id" .= i]
    toJSON (InnerCell b n) = object ["bbox" .= b, "node" .= n]

instance ToJSON (Node a) where
    toJSON = toJSON . nodeCells

data MaybeNodePair = forall a. MaybeNodePair (Node a) (Maybe (Node a))

nodeCells :: Node a -> V.Vector (Cell a)
nodeCells (LeafNode v) = v
nodeCells (InnerNode v) = v

mkNode :: V.Vector (Cell a) -> Node a
mkNode cells = case V.head cells of
    InnerCell _ _ -> InnerNode cells
    LeafCell _ _ -> LeafNode cells

instance Contained (Cell a) where
    bbox (LeafCell p _) = bbox p
    bbox (InnerCell b _) = b

instance Contained (Node a) where
    bbox = bbox . nodeCells

data Rtree  = forall p . Rtree BoundingBox (Node p)

instance Contained Rtree where
    bbox (Rtree b _) = b

instance ToJSON Rtree where
    toJSON (Rtree b root) = object ["bbox" .= b, "root" .= root]

mkInner :: Node a -> Cell Inner
mkInner c = InnerCell (bbox c) c

margin :: BoundingBox -> Int64
margin b = i max_x - i min_x + i max_y - i min_y where
    i = fromIntegral . unwrapCoord . ($ b)

area :: BoundingBox -> Int64
area b = (i max_x - i min_x)*(i max_y - i min_y) where
    i = fromIntegral . unwrapCoord . ($ b)

splits :: V.Vector (Cell a) -> [(Int, BoundingBox, BoundingBox)]
splits cells = let
    lhs_bboxes = drop min_node_fill $ scanl B.extend' B.empty $ V.toList cells
    rhs_bboxes_vec = V.scanr B.extend' B.empty cells
    rhs_bboxes = drop min_node_fill $ V.toList $ V.reverse rhs_bboxes_vec
    end = max_node_fill + 1 - min_node_fill
    in zip3 [min_node_fill..end] lhs_bboxes rhs_bboxes

chooseSplitAxis :: V.Vector (Cell a) -> V.Vector (Cell a)
chooseSplitAxis cells = let
    mkKey f g c = let b = bbox c in
        (unwrapCoord $ f b, unwrapCoord $ g b)
    keyX = mkKey min_x max_x
    keyY = mkKey min_y max_y

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

splitCells :: V.Vector (Cell a) -> (Node a, Node a)
splitCells cells = let
    cells' = chooseSplitAxis cells
    (k, _, _) = minimumBy (compareOn f) (splits cells') where
        f (_, lhs, rhs) = (
            area (B.intersection lhs rhs), area (B.extend lhs rhs))
    (lhs, rhs) = V.splitAt k cells'
    in (mkNode lhs, mkNode rhs)

insertAt :: Node a -> Cell a -> MaybeNodePair
insertAt n c = let
    cells' = V.snoc (nodeCells n) c
    in if V.length cells' <= max_node_fill
        then MaybeNodePair (mkNode cells') Nothing
        else let (x, y) = splitCells cells' in
            MaybeNodePair x (Just y)

compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn f x y = (f x) `compare` (f y)

chooseSubtree :: Node Inner -> BoundingBox -> Int
chooseSubtree n box = let
    cells = nodeCells n
    cellOverlap b index = V.ifoldr f 0 cells where
        f i c s
            | index == i = s
            | otherwise = s + area (B.intersection b (bbox c))

    metric :: Int -> BoundingBox -> Int64
    metric = case V.head cells of
        InnerCell _ node ->
            case node of
                -- Choose the cell that needs least overlap enlargement.
                LeafNode _ -> \k b ->
                    cellOverlap (B.extend box b) k - cellOverlap b k
                -- Choose the cell that needs least area enlargement.
                InnerNode _ -> \_ b -> area (B.extend box b) - area b

    cellMetric :: Int -> Int64
    cellMetric k = case cells ! k of
        InnerCell b _ -> metric k b

    in minimumBy (compareOn cellMetric) [0..V.length cells - 1]

insertInSubtree :: Node a -> Cell Leaf -> MaybeNodePair
insertInSubtree n@(LeafNode _) c = insertAt n c
insertInSubtree n@(InnerNode cs) c = let
    k = chooseSubtree n (bbox c)
    in case cs ! k of
        InnerCell _ child_node -> case insertInSubtree child_node c of
            MaybeNodePair new_child new_child_sibling' -> let
                node' = InnerNode $ cs // [(k, mkInner new_child)]
                in case new_child_sibling' of
                    Nothing -> MaybeNodePair node' Nothing
                    Just new_child_sibling ->
                        insertAt node' (mkInner new_child_sibling)

empty :: Rtree
empty = Rtree B.empty (LeafNode V.empty)

insert :: Point -> Int32 -> Rtree -> Rtree
insert p i (Rtree r_bbox r_root) = let
    new_bbox' = B.extend r_bbox (bbox p)
    new_bbox = new_bbox' `seq` new_bbox'
    in case insertInSubtree r_root (LeafCell p i) of
        MaybeNodePair r Nothing -> Rtree new_bbox r
        MaybeNodePair new_root (Just new_root_sibling) ->
            Rtree new_bbox (InnerNode $
                V.fromList [mkInner new_root, mkInner new_root_sibling])

lookup :: BoundingBox -> Rtree -> [Int32]
lookup query_bbox (Rtree r_bbox root) = go r_bbox root where
    go :: BoundingBox -> Node a -> [Int32]
    go node_bbox node
        | not (B.intersects query_bbox node_bbox) = []
        | otherwise = concatMap goCell (V.toList $ nodeCells node)
    goCell :: Cell a -> [Int32]
    goCell (LeafCell p i) = if B.contains query_bbox p then [i] else []
    goCell (InnerCell node_bbox node) = go node_bbox node
