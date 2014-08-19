{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Category.Free where

import Control.Applicative
import Control.Category
import Prelude hiding (id,(.))

data Free (t :: k -> k -> *) :: k -> k -> * where
  Id    ::                        Free t a a
  (:.:) :: t b c -> Free t a b -> Free t a c
infixr 4 :.:

instance Category (Free t) where
  id      = Id
  bd . ab = case bd of
    Id        -> ab
    cd :.: bc -> cd :.: bc . ab

type f :-> g = forall x. f x -> g x

class FreeCategory (r :: k -> k -> *) (s :: k -> k -> *) where
  fid :: r a a
  (&) :: Free s b c -> Free s a b -> r a c
  infixr 9 &

instance FreeCategory (Free r) r where
  fid = Id
  (&) = (.)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 1 .:

{-
finalize :: FreeCategory r s => Free s a b -> r a b
finalize t = case t of
  Id        -> fid
  bc :.: ab -> bc & finalize ab

instance FreeCategory (->) (->) where
  fid = id
  bc & ab = finalize bc . finalize ab
-}

{-
newtype Indexed i a b = Indexed
  { runIndexed :: i -> a -> b
  }
-}

{-
instance FreeCategory (Indexed i) (Indexed i) where
  fid     = Indexed $ const id
  bc & ab = Indexed $
    (.) <$> runIndexed (finalize bc)
        <*> runIndexed (finalize ab)
    
-}

