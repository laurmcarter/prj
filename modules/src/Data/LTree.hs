{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.LTree where

import qualified Prelude
import Prelude hiding (null, filter, map, foldl, foldr)

import Data.Sequence1 (Sequence1)
import qualified Data.Sequence1 as S1

import Control.Applicative hiding (empty)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Foldable as F
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

data LNode k a = LNode
  { nodeLabel :: a
  , nodeChildren :: LTree k a
  } deriving (Eq,Ord,Show)

instance Functor (LNode k) where
  fmap f (LNode a t) = LNode (f a) $ fmap f t

type PresentNode k = LNode k Bool

type Trail a = Bool -> Maybe a -> a
type Prune k a = LNode k a -> Maybe (LNode k a)

-- Queries {{{

nodeNull :: PresentNode k -> Bool
nodeNull (LNode a t) = not a && null t

nodeSize :: Num a => PresentNode k -> a
nodeSize (LNode a t) = (if a then (+ 1) else id) $ size t

nodeMember :: (Ord k, Sequence1 s) => Maybe (s k) -> PresentNode k -> Bool
nodeMember ms (LNode a t) = case ms of
  Just s -> member s t
  _ -> a

-- }}}

-- Construction {{{

singletonNodeWith :: (Sequence1 s1) => Trail a -> Maybe (s1 k) -> LNode k a
singletonNodeWith mkA ms = case ms of
  Just s' -> LNode (mkA False Nothing) $ singletonWith mkA s'
  _       -> LNode (mkA True  Nothing)   empty

nodeInsertWith :: (Ord k, Sequence1 s1) => Trail a -> Maybe (s1 k) -> LNode k a -> LNode k a
nodeInsertWith mkA ms (LNode a t) = case ms of
  Just s -> LNode (mkA False $ Just a) $ insertWith mkA s t
  _      -> LNode (mkA True  $ Just a) t

nodeDeleteWith :: (Ord k, Sequence1 s1) => Prune k a -> Trail a -> Maybe (s1 k) -> LNode k a -> Maybe (LNode k a)
nodeDeleteWith pr mkA ms (LNode a t) = pr
  $ case ms of
      Just s -> LNode (mkA False $ Just a) $ deleteWith pr mkA s t
      _      -> LNode (mkA True  $ Just a) t

-- }}}

-- Combine {{{

nodeUnionWith :: (Ord k) => (a -> a -> a) -> LNode k a -> LNode k a -> LNode k a
nodeUnionWith f (LNode a1 t1) (LNode a2 t2) = LNode (f a1 a2) $ unionWith f t1 t2

nodeDifferenceWith :: (Ord k)
  => (LNode k a -> Maybe (LNode k a)) -> (a -> a -> a)
  -> LNode k a -> LNode k a -> Maybe (LNode k a)
nodeDifferenceWith pr f (LNode a1 t1) (LNode a2 t2) = pr $ LNode (f a1 a2) $ differenceWith pr f t1 t2

nodeIntersectionWith :: (Ord k) => (a -> a -> a) -> LNode k a -> LNode k a -> LNode k a
nodeIntersectionWith f (LNode a1 t1) (LNode a2 t2) = LNode (f a1 a2) $ intersectionWith f t1 t2

-- }}}

-- Filter {{{

{-
nodeLeaves :: (Ord k) => PresentNode k -> Maybe (PresentNode k)
nodeLeaves n@(LNode _ t) = removeNull
  $ if null t
      then n
      else vacantNode $ leaves t
-}

{-
nodeParents :: (Ord k) => PresentNode k -> Maybe (PresentNode k)
nodeParents (LNode _ t) = removeNull
  $ LNode (hasChildren t)
  $ parents t
-}

removeNull :: PresentNode k -> Maybe (PresentNode k)
removeNull n = if nodeNull n
  then Nothing
  else Just n

-- }}}



newtype LTree k a = LTree
  { getNodes:: Map k (LNode k a)
  } deriving (Eq,Ord,Show)

instance Functor (LTree k) where
  fmap f (LTree m) = LTree $ fmap (fmap f) m

type PresentTree k = LTree k Bool

-- Queries {{{

null :: LTree k a -> Bool
null = M.null . getNodes

size :: Num a => PresentTree k -> a
size = getSum . F.foldMap (Sum . nodeSize) . getNodes

member :: (Ord k, Sequence1 s) => s k -> PresentTree k -> Bool
member s (LTree m) = fromMaybe False $
  nodeMember ms <$> M.lookup k m
  where
  (k,ms) = S1.viewl s

-- }}}

-- Construction {{{

empty :: LTree k a
empty = LTree M.empty

singletonWith :: (Sequence1 s1) => Trail a -> s1 k -> LTree k a
singletonWith mkA s = LTree $ M.singleton k $ singletonNodeWith mkA ms
  where
  (k,ms) = S1.viewl s

insertWith :: (Ord k, Sequence1 s1) => Trail a -> s1 k -> LTree k a -> LTree k a
insertWith mkA = insertWith_
  where
  insertWith_ s (LTree m) = LTree $ M.alter goTree k m
    where
    goTree mn = Just $ case mn of
      Just n -> goNode n
      _      -> singletonNodeWith mkA ms
    goNode (LNode a t) = case ms of
      Just s' -> LNode (mkA False $ Just a) $ insertWith_ s' t
      _       -> LNode (mkA True  $ Just a) t
    (k,ms) = S1.viewl s

deleteWith :: (Ord k, Sequence1 s1) => Prune k a -> Trail a -> s1 k -> LTree k a -> LTree k a
deleteWith pr mkA = deleteWith_
  where
  deleteWith_ s (LTree m) = LTree $ M.alter goTree k m
    where
    goTree mn = mn >>= goNode
    goNode (LNode a t) = pr $ case ms of
      Just s' -> LNode (mkA False $ Just a) $ deleteWith_ s' t
      _      -> LNode (mkA True  $ Just a) t
    (k,ms) = S1.viewl s

-- }}}

-- Combine {{{

unionWith :: (Ord k) => (a -> a -> a) -> LTree k a -> LTree k a -> LTree k a
unionWith f (LTree m1) (LTree m2) = LTree $ M.unionWith (nodeUnionWith f) m1 m2

differenceWith :: (Ord k)
  => (LNode k a -> Maybe (LNode k a)) -> (a -> a -> a)
  -> LTree k a -> LTree k a -> LTree k a
differenceWith pr f (LTree m1) (LTree m2) = LTree $ M.differenceWith (nodeDifferenceWith pr f) m1 m2

intersectionWith :: (Ord k) => (a -> a -> a) -> LTree k a -> LTree k a -> LTree k a
intersectionWith f (LTree m1) (LTree m2) = LTree $ M.intersectionWith (nodeIntersectionWith f) m1 m2

union :: (Ord k) => PresentTree k -> PresentTree k -> PresentTree k
union = unionWith (||)

unions :: (Ord k) => [PresentTree k] -> PresentTree k
unions = F.foldl' union empty

difference :: (Ord k) => PresentTree k -> PresentTree k -> PresentTree k
difference = differenceWith removeNull $ \occ1 occ2 -> occ1 && not occ2

intersection :: (Ord k) => PresentTree k -> PresentTree k -> PresentTree k
intersection = intersectionWith (&&)

-- }}}

-- Filter {{{

inChildren :: Monoid r => (LNode k a -> r) -> LTree k a -> r
inChildren f (LTree m) = F.foldMap f m

hasChildren :: PresentTree k -> Bool
hasChildren = getAny . inChildren (Any . nodeLabel)

{-
leaves :: Modules -> Modules
leaves (Modules ms) = Modules $ M.mapMaybe nodeLeaves ms

parents :: Modules -> Modules
parents (Modules ms) = Modules $ M.mapMaybe nodeParents ms

hasChildren :: Modules -> Bool
hasChildren (Modules ms) = F.any nodeOccupied ms
-}

-- }}}



{-
makeIso ''Modules

-- Node {{{

instance Functor Node_ where
  fmap f (Node o a) = Node o (f a)

-- Queries {{{

nodeNull :: Node -> Bool
nodeNull (Node o c) = not o && null c

nodeSize :: Node -> Int
nodeSize (Node o c) = (if o then (+ 1) else id) $ size c

nodeMember :: Maybe ModulePath -> Node -> Bool
nodeMember mp (Node o ms) = case mp of
  Just p -> member p ms
  _ -> o

-- }}}

-- Construction {{{

occupiedNode, vacantNode :: a -> Node_ a
occupiedNode = Node True
vacantNode = Node False

leafNode :: Node
leafNode = occupiedNode empty

singletonNode :: Maybe ModulePath -> Node
singletonNode = maybe leafNode (vacantNode . singleton)

nodeInsert :: Maybe ModulePath -> Node -> Node
nodeInsert mp (Node o ms) = case mp of
  Just p -> Node o $ insert p ms
  _ -> occupiedNode ms

nodeDelete :: Maybe ModulePath -> Node -> Maybe Node
nodeDelete mp (Node o ms) = removeNull
  $ case mp of
      Just p -> Node o $ delete p ms
      _ -> vacantNode ms

removeNull :: Node -> Maybe Node
removeNull n = if nodeNull n
  then Nothing
  else Just n

-- }}}

-- Combine {{{

nodeUnion :: Node -> Node -> Node
nodeUnion = nodeUnionWith union

nodeUnionWith :: (a -> a -> a) -> Node_ a -> Node_ a -> Node_ a
nodeUnionWith f (Node o1 c1) (Node o2 c2) = Node (o1 || o2) $ f c1 c2

nodeDifference :: Node -> Node -> Maybe Node
nodeDifference (Node o1 c1) (Node o2 c2) = removeNull
  $ Node (o1 && not o2) $ difference c1 c2

nodeIntersection :: Node -> Node -> Node
nodeIntersection (Node o1 c1) (Node o2 c2) = Node (o1 && o2) $ intersection c1 c2

-- }}}

-- Filter {{{

nodeLeaves :: Node -> Maybe Node
nodeLeaves n@(Node _ ms) = removeNull
  $ if null ms
      then n
      else vacantNode $ leaves ms

nodeParents :: Node -> Maybe Node
nodeParents (Node _ ms) = removeNull
  $ Node (hasChildren ms)
  $ parents ms

-- }}}

-- }}}

-- Intermediate {{{

emptyModules1 :: Modules1
emptyModules1 = M.empty

mkNode1 :: Maybe ModulePath -> Node1
mkNode1 = maybe (occupiedNode S.empty) (vacantNode . S.singleton)

node1Union :: Node1 -> Node1 -> Node1
node1Union = nodeUnionWith S.union

-- }}}

-- Modules {{{

instance Monoid Modules where
  mempty = empty
  mappend = union

-- Queries {{{

null :: Modules -> Bool
null = M.null . _modules

size :: Modules -> Int
size (Modules ms) = getSum $ F.foldMap (Sum . nodeSize) ms

member :: ModulePath -> Modules -> Bool
member p (Modules ms) = fromMaybe False $
  nodeMember mp <$> M.lookup t ms
  where
  (t,mp) = descendPath p

-- }}}

-- Construction {{{

empty :: Modules
empty = Modules M.empty

singleton :: ModulePath -> Modules
singleton p = Modules $ M.singleton t $ singletonNode mp
  where
  (t,mp) = descendPath p

insert :: ModulePath -> Modules -> Modules
insert p (Modules ms) = Modules $ M.alter go t ms
  where
  (t,mp) = descendPath p
  go mn = Just $ case mn of
    Just n -> nodeInsert mp n
    _ -> singletonNode mp

delete :: ModulePath -> Modules -> Modules
delete p (Modules ms) = Modules $ M.alter go t ms
  where
  (t,mp) = descendPath p
  go :: Maybe Node -> Maybe Node
  go mn = mn >>= nodeDelete mp

-- }}}

-- Combine {{{

union :: Modules -> Modules -> Modules
union (Modules m1) (Modules m2) = Modules $ M.unionWith nodeUnion m1 m2

unions :: [Modules] -> Modules
unions = F.foldl' union empty

difference :: Modules -> Modules -> Modules
difference (Modules m1) (Modules m2) = Modules $ M.differenceWith nodeDifference m1 m2

intersection :: Modules -> Modules -> Modules
intersection (Modules m1) (Modules m2) = Modules $ M.intersectionWith nodeIntersection m1 m2

-- }}}

-- Filter {{{

filter :: (ModulePath -> Bool) -> Modules -> Modules
filter pr = filter_ Sq.empty
  where
  filter_ :: Seq Text -> Modules -> Modules
  filter_ ps = under modules $ M.mapMaybeWithKey go
    where
    go :: Text -> Node -> Maybe Node
    go t (Node o c) = removeNull $ Node (o && pr p) (filter_ (ps Sq.|> t) c)
      where
      p :: ModulePath
      p = mkModule ps t

leaves :: Modules -> Modules
leaves (Modules ms) = Modules $ M.mapMaybe nodeLeaves ms

parents :: Modules -> Modules
parents (Modules ms) = Modules $ M.mapMaybe nodeParents ms

hasChildren :: Modules -> Bool
hasChildren (Modules ms) = F.any nodeOccupied ms

-- }}}

-- Conversion {{{

toList :: Modules -> [ModulePath]
toList = foldr (:) []

fromList :: [ModulePath] -> Modules
fromList = bundleModules . S.fromList

-- }}}

-- Ordered List {{{

toAscList :: Modules -> [ModulePath]
toAscList = toList

toDescList :: Modules -> [ModulePath]
toDescList = foldl (flip (:)) []

-- }}}

-- Map {{{

map :: (ModulePath -> ModulePath) -> Modules -> Modules
map f = fromList . Prelude.map f . toList

-- }}}

-- Folds {{{

foldl :: forall a. (a -> ModulePath -> a) -> a -> Modules -> a
foldl f = foldl_ Sq.empty
  where
  foldl_ :: Seq Text -> a -> Modules -> a
  foldl_ ps z (Modules ms) = M.foldlWithKey go z ms
    where
    go :: a -> Text -> Node -> a
    go z' t (Node o ms') = foldl_ (ps Sq.|> t) (if o then f z' p else z') ms'
      where
      p = mkModule ps t

foldlAll :: forall a. (a -> ModulePath -> Bool -> Modules -> a) -> a -> Modules -> a
foldlAll f = foldlAll_ Sq.empty
  where
  foldlAll_ :: Seq Text -> a -> Modules -> a
  foldlAll_ ps z (Modules ms) = M.foldlWithKey go z ms
    where
    go :: a -> Text -> Node -> a
    go z' t (Node o ms') = foldlAll_ (ps Sq.|> t) (f z' p o ms') ms'
      where
      p = mkModule ps t

foldr :: forall a. (ModulePath -> a -> a) -> a -> Modules -> a
foldr f = foldr_ Sq.empty
  where
  foldr_ :: Seq Text -> a -> Modules -> a
  foldr_ ps z (Modules ms) = M.foldrWithKey go z ms
    where
    go :: Text -> Node -> a -> a
    go t (Node o ms') z' = (if o then f p else id) $ foldr_ (ps Sq.|> t) z' ms'
      where
      p = mkModule ps t

foldrAll :: forall a. (ModulePath -> Bool -> Modules -> a -> a) -> a -> Modules -> a
foldrAll f = foldrAll_ Sq.empty
  where
  foldrAll_ :: Seq Text -> a -> Modules -> a
  foldrAll_ ps z (Modules ms) = M.foldrWithKey go z ms
    where
    go :: Text -> Node -> a -> a
    go t (Node o ms') z' = f p o ms' $ foldrAll_ (ps Sq.|> t) z' ms'
      where
      p = mkModule ps t

foldMap :: forall m. (Monoid m) => (ModulePath -> m) -> Modules -> m
foldMap f = foldr (mappend . f) mempty

foldMapAll :: forall m. (Monoid m) => (ModulePath -> Bool -> Modules -> m)
  -> Modules -> m
foldMapAll f = foldrAll (\p oc ms -> (f p oc ms <>)) mempty

_Modules :: Fold Modules ModulePath
_Modules = to toList . folded

-- }}}

-- Traverse {{{

traverse_ :: forall f a. (Applicative f) => (ModulePath -> f a) -> Modules -> f ()
traverse_ f = foldr go $ pure ()
  where
  go :: ModulePath -> f () -> f ()
  go p = (f p *>)

-- }}}

-- Building Helpers {{{

bundleModules :: Set ModulePath -> Modules
bundleModules = Modules . fmap (fmap bundleModules) . commonPrefixes

commonPrefixes :: Set ModulePath -> Modules1
commonPrefixes = F.foldr add emptyModules1
  where
  add :: ModulePath -> Modules1 -> Modules1
  add = graft . fmap mkNode1 . descendPath
  graft :: (Text,Node1) -> Modules1 -> Modules1
  graft = uncurry $ M.insertWith node1Union

-- }}}

-- }}}
-}

