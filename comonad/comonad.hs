
import Control.Comonad
import Control.Applicative
import Data.Maybe

data Point a = Point !(Maybe (Point a)) !a !(Maybe (Point a))

instance Show a => Show (Point a) where
  show p@(Point ml a mr) = sh (isJust ml) (isJust mr) p

sh :: Show a => Bool -> Bool -> Point a -> String
sh l r (Point ml a mr) =
  (case ml of
     Just p  -> if l then sh True False p else ""
     Nothing -> "|") ++
  show a ++
  (case mr of
     Just p  -> if r then sh False True p else ""
     Nothing -> "|")

instance Functor Point where
  fmap f (Point ml a mr) = Point (fmap (fmap f) ml) (f a) (fmap (fmap f) mr)

instance Comonad Point where
  extract (Point _ a _) = a
  duplicate p@(Point ml a mr) = Point (fmap duplicate ml) p (fmap duplicate mr)

left :: Point a -> Point a
left p@(Point ml _ _) = fromMaybe p ml

right :: Point a -> Point a
right p@(Point _ _ mr) = fromMaybe p mr

data Cell = O | X

instance Show Cell where
  show c = case c of
    O -> "O"
    X -> " "

row :: [Cell] -> Point Cell
row [] = error "empty list"
row (c:cs) = let p = Point Nothing c (row' cs p) in p
  where
  row' :: [Cell] -> Point Cell -> Maybe (Point Cell)
  row' [] _     = Nothing
  row' (c:cs) p = let p' = Point (Just p) c (row' cs p') in Just p'

testRow = row (replicate 25 X ++ [O] ++ replicate 25 X)

nextRow :: Point Cell -> Point Cell
nextRow = extend rule30

rule30 :: Point Cell -> Cell
rule30 (Point ml a mr) =
  case (l,a,r) of
    (X,X,X) -> X
    (X,X,O) -> O
    (X,O,X) -> O
    (X,O,O) -> O
    (O,X,X) -> O
    (O,X,O) -> X
    (O,O,X) -> X
    (O,O,O) -> X
  where 
    l = fromMaybe X $ extract <$> ml
    r = fromMaybe X $ extract <$> mr

auto :: Int -> Point Cell -> IO ()
auto n p
  | n < 1 = return ()
  | otherwise = do
    print p
    let p' = nextRow p
    auto (n-1) p'

main :: IO ()
main = auto 100 testRow

