{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Graph.Initial where

import Prelude hiding (lookup)
import Control.Lens
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Bifunctor
import Data.Monoid
import Data.Char

type NodeKey = I.Key
type EdgeKey = I.Key


-- Pair {{{

data Pair a = a :*: a deriving (Show)
infixl 4 :*:

mkPair :: Ord a => a -> a -> Pair a
mkPair = curry $ view pair

pair :: Ord a => Iso (a,a) (b,b) (Pair a) (Pair b)
pair = flip iso (\(a :*: b) -> (a,b)) $ \case
  (a,b) | a <= b -> a :*: b
        | True   -> b :*: a

repair :: Ord a => Pair a -> Pair a
repair = view $ from pair . pair

instance Ord a => Eq (Pair a) where
  (repair -> a :*: b) == (repair -> c :*: d) = a == c && b == d

instance Ord a => Ord (Pair a) where
  compare (repair -> a :*: b) (repair -> c :*: d) = compare a c <> compare b d

-- }}}


data Graph g e n = Graph
  { _graphLabel :: g
  , _graphEdges :: IntMap (e n,Pair n)
  , _graphNodes :: IntMap n
  } deriving (Eq,Show)

makeLenses ''Graph

type BuildT g e n m = StateT (Graph g e n) m
type Build  g e n   = State  (Graph g e n)


runGraphT :: Monad m => g -> BuildT g e n m a -> m (a,Graph g e n)
runGraphT g m = runStateT m $ empty g

runGraph :: g -> Build g e n a -> (a,Graph g e n)
runGraph g m = runState m $ empty g

runGraphT_ :: Monad m => BuildT () e n m a -> m (a,Graph () e n)
runGraphT_ = runGraphT ()

runGraph_ :: Build () e n a -> (a,Graph () e n)
runGraph_ = runGraph ()

buildGraphT :: Monad m => g -> BuildT g e n m () -> m (Graph g e n)
buildGraphT g m = execStateT m $ empty g

buildGraph :: g -> Build g e n () -> Graph g e n
buildGraph g m = execState m $ empty g

buildGraphT_ :: Monad m => BuildT () e n m () -> m (Graph () e n)
buildGraphT_ = buildGraphT ()

buildGraph_ :: Build () e n () -> Graph () e n
buildGraph_ = buildGraph ()

empty :: g -> Graph g e n
empty g = Graph g mempty mempty

done :: Monad m => BuildT g e n m ()
done = return ()

node :: (Monad m, Ord n) => n -> BuildT g e n m NodeKey
node n = do
  k <- use nextNodeKey
  mn <- use $ _NodeKey n
  whenJust mn $ const $ fail "node: duplicate"
  graphNodes %= I.insert k n
  return k

edge :: (Monad m, Ord n, Ord (e n))
  => (n -> n -> e n) -> NodeKey -> NodeKey
  -> BuildT g e n m EdgeKey
edge f k1 k2 = do
  k  <- use nextEdgeKey
  mn1 <- use $ _Node k1
  mn2 <- use $ _Node k2
  case (mn1,mn2) of
    (Just n1,Just n2) -> do
      let e = f n1 n2
          p = mkPair n1 n2
      me <- use $ _EdgeKey e p
      whenJust me $ const $ fail "edge: duplicate"
      graphEdges %= I.insert k (e,p)
      return k
    (_      ,Just _ ) -> noNode k1
    (Just _ ,_      ) -> noNode k2
    _                 -> noNodes
  where
  noNode i = fail $ "edge: Node "++show i++" not in graph"
  noNodes  = fail $ "edge: Nodes "++show k1++" and "++show k2++" not in graph"

deleteNode :: Monad m => NodeKey -> BuildT g e n m (Maybe n)
deleteNode k = _Node k <<%= const Nothing

deleteEdge :: Monad m => EdgeKey -> BuildT g e n m (Maybe (e n,Pair n))
deleteEdge k = _Edge k <<%= const Nothing

-- Key Util {{{

nextKey :: IntMap a -> I.Key
nextKey m = go 0
  where
  go i = if I.member i m
    then go $ i+1
    else i

nextNodeKey :: Ord n => IndexPreservingGetter (Graph g e n) NodeKey
nextNodeKey = cloneIndexPreservingLens graphNodes . to nextKey

nextEdgeKey :: IndexPreservingGetter (Graph g e n) EdgeKey
nextEdgeKey = cloneIndexPreservingLens graphEdges . to nextKey

-- }}}

-- Nodes {{{

_NodeKeys :: Ord n => IndexedTraversal' n (Graph g e n) NodeKey
_NodeKeys = graphNodes . from _KeyMap . mirrored . itraversed

_NodeKey :: Ord n => n -> Lens' (Graph g e n) (Maybe NodeKey)
_NodeKey n = graphNodes . from _KeyMap . mirrored . at n

_Nodes :: IndexedTraversal' NodeKey (Graph g e n) n
_Nodes = graphNodes . itraversed

_Node :: NodeKey -> Lens' (Graph g e n) (Maybe n)
_Node k = graphNodes . at k

-- }}}

-- Edges {{{

_EdgeKeys :: (Ord n, Ord (e n))
  => IndexedTraversal' (e n,Pair n) (Graph g e n) EdgeKey
_EdgeKeys = graphEdges . from _KeyMap . mirrored . itraversed

_EdgeKey :: (Ord n, Ord (e n))
  => e n -> Pair n -> Lens' (Graph g e n) (Maybe EdgeKey)
_EdgeKey e p = graphEdges . from _KeyMap . mirrored . at (e,p)

_EdgeMap :: IndexedTraversal' EdgeKey (Graph g e n) (e n,Pair n)
_EdgeMap = graphEdges . itraversed

_Edges :: IndexedTraversal' (EdgeKey,Pair n) (Graph g e n) (e n)
_Edges = _EdgeMap <.> _Edge1

_Edge :: EdgeKey -> Lens' (Graph g e n) (Maybe (e n,Pair n))
_Edge k = graphEdges . at k

_Edge' :: EdgeKey -> IndexedTraversal' (EdgeKey,Pair n) (Graph g e n) (e n)
_Edge' k = _EdgeMap . index k <.> _Edge1

_Edge1 :: IndexedLens' (Pair n) (e n,Pair n) (e n)
_Edge1 = ilens
  (\(e,p) -> (p,e))
  (\(_,p) e -> (e,p))

-- }}}


-- Maps {{{

mirrored :: (Ord a, Ord b, Ord c, Ord d)
  => Iso (Map a b) (Map c d) (Map b a) (Map d c)
mirrored = _Map . mapping swapped . from _Map

_Map :: (Ord a, Ord c) => Iso (Map a b) (Map c d) [(a,b)] [(c,d)]
_Map = _Wrapped

_IntMap :: Iso (IntMap a) (IntMap b) [(Int,a)] [(Int,b)]
_IntMap = _Wrapped

_KeyMap :: Iso (Map Int a) (Map Int b) (IntMap a) (IntMap b)
_KeyMap = _Map . from _IntMap

-- }}}

-- Util {{{

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

-- }}}

-- Examples {{{

data Dir n
  = ToLeft
  | ToRight
  deriving (Eq,Ord,Show)

(<--) :: (Monad m, Ord n) => NodeKey -> NodeKey -> BuildT g Dir n m EdgeKey
(<--) = edge $ \_ _ -> ToLeft

(-->) :: (Monad m, Ord n) => NodeKey -> NodeKey -> BuildT g Dir n m EdgeKey
(-->) = edge $ \_ _ -> ToRight

(<-->) :: (Monad m, Ord n) => NodeKey -> NodeKey -> BuildT g Dir n m (EdgeKey,EdgeKey)
n1 <--> n2 = liftM2 (,) (n1 <-- n2) (n1 --> n2)

e0 :: Build g Dir String ()
e0 = do
  n1 <- node "foo"
  n2 <- node "bar"
  n1 <--> n2
  n3 <- node "baz"
  n2 --> n3
  done

-- }}}

  -- me <- use $ _Edge k
  -- whenJust me $ const $ graphEdges %= I.delete k
  -- return me
