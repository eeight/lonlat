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

import Control.Monad.ST(runST)
import Data.Aeson(ToJSON(..), (.=), object)
import Data.Int(Int32, Int64)
import Data.List(minimumBy, foldl')
import Data.Monoid(First(..))
import Data.Ord(comparing)
import Data.Vector((!), (//))
import GHC.TypeLits
import Prelude hiding (lookup)
import Unsafe.Coerce(unsafeCoerce)

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

maxNodeFill :: Int
maxNodeFill = 9

minNodeFill :: Int
minNodeFill = 3

data Node m where
    Leaf :: Point -> Int32 -> Node 0
    Inner :: Int -> BoundingBox -> V.Vector (Node m) -> Node (m + 1)

data AnyNode = forall m . AnyNode (Node m)
data Rtree = forall m . T (Maybe (Node m))

data Context m n where
    Root :: Context n n
    Hole :: Int -> V.Vector (Node n) -> Context m (n + 1) -> Context m n

data Zipper m n = Zipper (Node n) (Context m n)

data AnyZipper :: Nat -> * where
    AnyZipper :: Zipper m n -> AnyZipper m

instance ToJSON (Node m) where
    toJSON (Leaf p i) = object ["point" .= p, "id" .= i]
    toJSON (Inner _ box ns) = object ["bbox" .= box, "children" .= ns]

instance ToJSON Rtree where
    toJSON (T x) = maybe (object []) toJSON x

instance Contained (Node m) where
    bbox (Leaf p _) = bbox p
    bbox (Inner _ b _) = b

instance Contained Rtree where
    bbox (T x) = maybe B.empty bbox x

height :: Node m -> Int
height Leaf{} = 0
height (Inner n _ _) = n

mkZipper :: Node m -> Zipper m m
mkZipper = flip Zipper Root

zipUp :: Zipper m n -> Node m
zipUp (Zipper c Root) = c
zipUp (Zipper n (Hole index ns ctx)) = let
    ns' = ns // [(index, n)]
    in zipUp $ Zipper (mkInner ns') ctx

zipStepDown :: Int -> V.Vector (Node n) -> Context m (n + 1) -> Zipper m n
zipStepDown k ns c = Zipper (ns ! k) (Hole k ns c)

unzipToInsert :: Zipper m n -> BoundingBox -> Zipper m 0
unzipToInsert z@(Zipper (Leaf _ _) _) _ = z
unzipToInsert (Zipper (Inner _ _ ns) c) box = let
    k = chooseSubtree ns box
    in unzipToInsert (zipStepDown k ns c) box

unzipToDelete :: Zipper m n -> Point -> Int32 -> Maybe (Zipper m 0)
unzipToDelete z@(Zipper (Leaf lp li) _) p i =
    if (lp, li) == (p, i) then Just z else Nothing
unzipToDelete (Zipper (Inner _ box ns) c) p i
    | not $ B.contains box p = Nothing
    | otherwise = getFirst $ mconcat [
        First $ unzipToDelete (zipStepDown k ns c) p i | k <- [0..V.length ns - 1]]

deleteAt :: Context m n -> Rtree
deleteAt c = let
    (r, excessNodes) = deleteAtAcc c []
    in foldl' insertAnyNode r excessNodes
  where
    deleteAtAcc :: Context m n -> [AnyNode] -> (Rtree, [AnyNode])
    deleteAtAcc Root acc = (T Nothing, acc)
    deleteAtAcc (Hole k ns ctx) acc = let
        ns' = let (a, b) = V.splitAt k ns in (V.++) a (V.tail b)
        in if V.length ns' < maxNodeFill
            then deleteAtAcc ctx (acc ++ map AnyNode (V.toList ns'))
            else (T $ Just $ zipUp $ Zipper (mkInner ns') ctx, acc)

    coerceHeight :: Node a -> Node b
    coerceHeight = unsafeCoerce

    insertAnyNode :: Rtree -> AnyNode -> Rtree
    insertAnyNode (T Nothing) n = case n of AnyNode n' -> T (Just n')
    insertAnyNode (T (Just r)) n = case n of
        AnyNode n' -> case unzipToInsertHeight (mkZipper r) (bbox n') (height n') of
            AnyZipper (Zipper n2 c) -> insertAt n2 (coerceHeight n') c

    unzipToInsertHeight :: Zipper m n -> BoundingBox -> Int -> AnyZipper m
    unzipToInsertHeight z@(Zipper (Leaf{}) _) _ _ = AnyZipper z
    unzipToInsertHeight z@(Zipper n _) _ h | height n == h = AnyZipper z
    unzipToInsertHeight (Zipper (Inner _ _ ns) c) box h = let
        k = chooseSubtree ns box
        in unzipToInsertHeight (zipStepDown k ns c) box h

mkInner :: V.Vector (Node n) -> Node (n + 1)
mkInner nodes = Inner (height (V.head nodes) + 1) (bbox nodes) nodes

margin :: BoundingBox -> Int64
margin b = i max_x - i min_x + i max_y - i min_y where
    i = fromIntegral . unwrapCoord . ($ b)

area :: BoundingBox -> Int64
area b = (i max_x - i min_x)*(i max_y - i min_y) where
    i = fromIntegral . unwrapCoord . ($ b)

chooseSubtree :: V.Vector (Node m) -> BoundingBox -> Int
chooseSubtree nodes box = let
    nodeOverlap b index = V.ifoldr f 0 nodes where
        f i n acc
            | index == i = acc
            | otherwise = acc + area (B.intersection b (bbox n))

    metric :: Int -> BoundingBox -> Int64
    metric = case height (V.head nodes) of
        0 -> \_ _ -> 0
        1 -> \k b -> nodeOverlap (B.extend box b) k - nodeOverlap b k
        _ -> \_ b -> area (B.extend box b) - area b

    nodeMetric :: Int -> Int64
    nodeMetric k = metric k (bbox $ nodes ! k)

    in minimumBy (comparing nodeMetric) [0..V.length nodes - 1]

splits :: V.Vector (Node m) -> [(Int, BoundingBox, BoundingBox)]
splits nodes = let
    lhs_bboxes = drop minNodeFill $ scanl B.extend' B.empty $ V.toList nodes
    rhs_bboxes_vec = V.scanr B.extend' B.empty nodes
    rhs_bboxes = drop minNodeFill $ V.toList $ V.reverse rhs_bboxes_vec
    end = maxNodeFill + 1 - minNodeFill
    in zip3 [minNodeFill..end] lhs_bboxes rhs_bboxes

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
        VI.sortBy (comparing key) vm
        V.freeze vm

    nodes'x = sortBy keyX nodes
    margin'x = marginForSplits nodes'x

    nodes'y = sortBy keyY nodes
    margin'y = marginForSplits nodes'y
    in if margin'x < margin'y then nodes'x else nodes'y

splitNodes :: V.Vector (Node m) -> (Node (m + 1), Node (m + 1))
splitNodes nodes = let
    nodes' = chooseSplitAxis nodes
    (k, _, _) = minimumBy (comparing f) (splits nodes') where
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
    in if V.length nodes' < maxNodeFill
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
    go (Leaf p i) = [i | B.contains query_bbox p]
    go (Inner _ node_bbox ns)
        | not (B.intersects query_bbox node_bbox) = []
        | otherwise = concatMap go (V.toList ns)

delete :: Point -> Int32 -> Rtree -> Rtree
delete _ _ r@(T Nothing) = r
delete p i r@(T (Just n)) = case unzipToDelete (mkZipper n) p i of
    Just (Zipper (Leaf _ i') c) | i == i' ->deleteAt c
    _ -> r
