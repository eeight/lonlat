module Rtree
    ( Rtree
    , empty
    , insert
    , lookup
    , delete
    ) where

import Coord(Coord(..))
import Point
import BoundingBox(BoundingBox(..), Contained(..))

import qualified BoundingBox as B

import Prelude hiding (lookup, (++))
import Control.Monad.ST(runST)
import Data.Aeson(ToJSON(..), (.=), object)
import Data.Int(Int32, Int64)
import Data.List(minimumBy)
import Data.Vector((!), (//), (++))

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

max_node_fill :: Int
max_node_fill = 9

min_node_fill :: Int
min_node_fill = 3

data Nat = Zero | Succ Nat

data Node m where
    Leaf :: Point -> Int32 -> Node Zero
    Inner :: BoundingBox -> V.Vector (Node m) -> Node (Succ m)

data Rtree = forall m . T (Maybe (Node m))

data Context m n where
    Root :: Context n n
    Hole :: Int -> V.Vector (Node n) -> Context m (Succ n) -> Context m n

data Zipper m n = Zipper (Node n) (Context m n)

instance ToJSON (Node m) where
    toJSON (Leaf p i) = object ["point" .= p, "id" .= i]
    toJSON (Inner box ns) = object ["bbox" .= box, "children" .= ns]

instance ToJSON Rtree where
    toJSON (T x) = maybe (object []) toJSON x

instance Contained (Node m) where
    bbox (Leaf p _) = bbox p
    bbox (Inner b _) = b

instance Contained Rtree where
    bbox (T x) = maybe B.empty bbox x

mkZipper :: Node m -> Zipper m m
mkZipper = flip Zipper Root

zipUp :: Zipper m n -> Node m
zipUp (Zipper c Root) = c
zipUp (Zipper n (Hole index ns ctx)) = let
    ns' = ns // [(index, n)]
    in zipUp $ Zipper (mkInner ns') ctx

zipStepDown :: Int -> V.Vector (Node n) -> Context m (Succ n) -> Zipper m n
zipStepDown k ns c = Zipper (ns ! k) (Hole k ns c)

unzipToInsert :: Zipper m n -> BoundingBox -> Zipper m Zero
unzipToInsert z@(Zipper (Leaf _ _) _) _ = z
unzipToInsert (Zipper (Inner _ ns) c) box = let
    k = chooseSubtree ns box
    in unzipToInsert (zipStepDown k ns c) box

unzipToDelete :: Zipper m n -> Point -> Int32 -> Maybe (Zipper m Zero)
unzipToDelete z@(Zipper (Leaf lp li) _) p i =
    if (lp, li) == (p, i) then Just z else Nothing
unzipToDelete (Zipper (Inner box ns) c) p i
    | not $ B.contains box p = Nothing
    | otherwise = msum [
        unzipToDelete (zipStepDown k ns c) p i | k <- [0..V.length ns - 1]]

data Height m where
    Here :: Node m -> Height m
    Below :: Height m -> Height (Succ m)

deleteAt :: Context m n -> Rtree
deleteAt c = let
    (r, nodes) = go c []
    in foldl' insertNode rtree nodes
  where
    go :: Context m n -> (Rtree, [Height m])
    go Root acc = (T Nothing, acc)
    go (Hole k ns ctx) acc = let
        ns' = let (a, b) = V.splitAt k ns in a ++ V.tail b
        in if V.length ns' < max_node_fill
            then T $ Just $ go ctx (acc ++ V.toList ns')
            else (T $ Just $ zipUp $ Zipper (mkInner ns') ctx, acc)

mkInner :: V.Vector (Node n) -> Node (Succ n)
mkInner nodes = Inner (bbox nodes) nodes

margin :: BoundingBox -> Int64
margin b = i max_x - i min_x + i max_y - i min_y where
    i = fromIntegral . unwrapCoord . ($ b)

area :: BoundingBox -> Int64
area b = (i max_x - i min_x)*(i max_y - i min_y) where
    i = fromIntegral . unwrapCoord . ($ b)

compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn f x y = (f x) `compare` (f y)

depth :: Node m -> Nat
depth (Leaf _ _) = Zero
depth (Inner _ ns) = Succ $ depth (V.head ns)

chooseSubtree :: V.Vector (Node m) -> BoundingBox -> Int
chooseSubtree nodes box = let
    nodeOverlap b index = V.ifoldr f 0 nodes where
        f i n acc
            | index == i = acc
            | otherwise = acc + area (B.intersection b (bbox n))

    metric :: Int -> BoundingBox -> Int64
    metric = case depth (V.head nodes) of
        Zero -> \_ _ -> 0
        (Succ Zero) -> \k b ->
            nodeOverlap (B.extend box b) k - nodeOverlap b k
        _ -> \_ b -> area (B.extend box b) - area b

    nodeMetric :: Int -> Int64
    nodeMetric k = metric k (bbox $ nodes ! k)

    in minimumBy (compareOn nodeMetric) [0..V.length nodes - 1]

splits :: V.Vector (Node m) -> [(Int, BoundingBox, BoundingBox)]
splits nodes = let
    lhs_bboxes = drop min_node_fill $ scanl B.extend' B.empty $ V.toList nodes
    rhs_bboxes_vec = V.scanr B.extend' B.empty nodes
    rhs_bboxes = drop min_node_fill $ V.toList $ V.reverse rhs_bboxes_vec
    end = max_node_fill + 1 - min_node_fill
    in zip3 [min_node_fill..end] lhs_bboxes rhs_bboxes

chooseSplitAxis :: V.Vector (Node m) -> V.Vector (Node m)
chooseSplitAxis nodes = let
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

    nodes'x = sortBy keyX nodes
    margin'x = marginForSplits nodes'x

    nodes'y = sortBy keyY nodes
    margin'y = marginForSplits nodes'y
    in if margin'x < margin'y then nodes'x else nodes'y

splitNodes :: V.Vector (Node m) -> (Node (Succ m), Node (Succ m))
splitNodes nodes = let
    nodes' = chooseSplitAxis nodes
    (k, _, _) = minimumBy (compareOn f) (splits nodes') where
        f (_, lhs, rhs) = (
            area (B.intersection lhs rhs), area (B.extend lhs rhs))
    (lhs, rhs) = V.splitAt k nodes'
    in (mkInner lhs, mkInner rhs)

empty :: Rtree
empty = T Nothing

insertAt :: Node n -> Node n -> Context m n -> Rtree
insertAt n1 n2 Root = T $ Just $
    mkInner (V.fromList [n1, n2])
insertAt n1 n2 (Hole k nodes ctx) = let
    nodes' = V.snoc (nodes // [(k, n1)]) n2
    in if V.length nodes' < max_node_fill
        then T $ Just $ zipUp $ Zipper (mkInner nodes') ctx
        else let (m1, m2) = splitNodes nodes' in insertAt m1 m2 ctx

insert :: Point -> Int32 -> Rtree -> Rtree
insert p i (T Nothing) = T $ Just $ Leaf p i
insert p i (T (Just n)) = case unzipToInsert (mkZipper n) (bbox p) of
    Zipper leaf c -> insertAt leaf (Leaf p i) c

lookup :: BoundingBox -> Rtree -> [Int32]
lookup _ (T Nothing) = []
lookup query_bbox (T (Just n)) = go n where
    go :: Node m -> [Int32]
    go (Leaf p i) = if B.contains query_bbox p then [i] else []
    go (Inner node_bbox ns)
        | not (B.intersects query_bbox node_bbox) = []
        | otherwise = concatMap go (V.toList $ ns)

delete :: Point -> Int32 -> Rtree -> Rtree
delete _ _ r@(T Nothing) = r
delete p i r@(T (Just n)) = case unzipToDelete (mkZipper n) p i of
    Just (Zipper (Leaf _ i') c) -> if i == i' then deleteAt c else r
    _ -> r
