
module PMap where

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data PMap k v
  = Empty
  | Node (PMap k v) k v (PMap k v) Int
  deriving (Eq,Show)

instance Functor (PMap k) where
  fmap f m = case m of
    Empty -> Empty
    Node l k v r h -> Node (f <$> l) k (f v) (f <$> r) h

instance F.Foldable (PMap k) where
  foldr f b m = case m of
    Empty -> b
    Node l k v r _ -> F.foldr f (f v (F.foldr f b r)) l

instance T.Traversable (PMap k) where
  traverse f m = case m of
    Empty -> pure Empty
    Node l k v r h -> Node <$> T.traverse f l <*> pure k <*> f v <*> T.traverse f r <*> pure h

height :: PMap k v -> Int
height pm = case pm of
  Empty -> 0
  Node _ _ _ _ h -> h

mkPMap :: PMap k v -> k -> v -> PMap k v -> PMap k v
mkPMap l k v r = Node l k v r (max (height l) (height r) + 1)

balance :: PMap k v -> k -> v -> PMap k v -> PMap k v
balance l k v r = if hl > hr + 2
  then case l of
    Empty -> badHeight
    Node ll lk lv lr _ ->
      if height ll >= height lr
      then mkPMap ll lk lv (mkPMap lr k v r)
      else case lr of
        Empty -> badHeight
        Node lrl lrk lrv lrr _ -> mkPMap (mkPMap ll lk lv lrl) lrk lrv (mkPMap lrr k v r)
  else if hr > hl + 2
    then case r of
      Empty -> badHeight
      Node rl rk rv rr _ ->
        if height rr >= height rl
        then mkPMap (mkPMap l k v rl) rk rv rr
        else case rl of
          Empty -> badHeight
          Node rll rlk rlv rlr _ ->
            mkPMap (mkPMap l k v rll) rlk rlv (mkPMap rlr rk rv rr)
    else Node l k v r (max hl hr + 1)
  where
  hl = height l
  hr = height r
  badHeight = error "bad height calculations in height."

findMin :: Ord k => PMap k v -> (k,v)
findMin m = case m of
  Empty -> error "findMin: Empty PMap"
  Node Empty k v _ _ -> (k,v)
  Node l _ _ _ _ -> findMin l

deleteFindMin :: PMap k v -> ((k,v),PMap k v)
deleteFindMin m = case m of
  Empty -> error "deleteFindMin: Empty PMap"
  Node Empty k v r _ -> ((k,v),r)
  Node l k v r _ -> let (b,l') = deleteFindMin l in
    (b,balance l' k v r)

deleteFindMax :: PMap k v -> ((k,v),PMap k v)
deleteFindMax m = case m of
  Empty -> error "deleteFindMax: Empty PMap"
  Node l k v Empty _ -> ((k,v),l)
  Node l k v r _ -> let (b,r') = deleteFindMax r in
    (b,balance l k v r')

merge :: PMap k v -> PMap k v -> PMap k v
merge m1 m2 = case (m1,m2) of
  (Empty,_) -> m2
  (_,Empty) -> m1
  _ -> let ((k,v),m2') = deleteFindMin m2 in
    balance m1 k v m2'

empty :: PMap k v
empty = Empty

isEmpty :: PMap k v -> Bool
isEmpty m = case m of
  Empty -> True
  _ -> False

add :: Ord k => k -> v -> PMap k v -> PMap k v
add x d m = case m of
  Empty -> Node Empty x d Empty 1
  Node l k v r h -> case compare x k of
    EQ -> Node l x d r h
    LT -> balance (add x d l) k v r
    GT -> balance l k v (add x d r)
    
insertWith :: Ord k => (v -> v -> v) -> k -> v -> PMap k v -> PMap k v
insertWith f x d m = case m of
  Empty -> Node Empty x d Empty 1
  Node l k v r h -> case compare x k of
      EQ -> Node l x (f d v) r h
      LT -> balance (insertWith f x d l) k v r
      GT -> balance l k v (insertWith f x d r)

find :: Ord k => k -> PMap k v -> Maybe v
find x m = case m of
  Empty -> Nothing
  Node l k v r _ -> case compare x k of
    EQ -> Just v
    LT -> find x l
    GT -> find x r

remove :: Ord k => k -> PMap k v -> PMap k v
remove x m = case m of
  Empty -> Empty
  Node l k v r _ -> case compare x k of
    EQ -> merge l r
    LT -> balance (remove x l) k v r
    GT -> balance l k v (remove x r)

mem :: Ord k => k -> PMap k v -> Bool
mem x m = case m of
  Empty -> False
  Node l k v r _ -> case compare x k of
    EQ -> True
    LT -> mem x l
    GT -> mem x r

mapi :: Ord k => (k -> v -> v) -> PMap k v -> PMap k v
mapi f m = case m of
  Empty -> Empty
  Node l k v r h -> Node (mapi f l) k (f k v) (mapi f r) h

foldi :: Ord k => (k -> v -> a -> a) -> a -> PMap k v -> a
foldi f acc m = case m of
  Empty -> acc
  Node l k v r h -> foldi f (f k v (foldi f acc l)) r

