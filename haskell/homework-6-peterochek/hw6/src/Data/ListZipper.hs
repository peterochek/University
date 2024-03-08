module Data.ListZipper
  ( ListZipper(..)
  , shiftLeft
  , shiftRight
  , shift
  , insert
  , truncateLZ
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a =
  LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ left el right) = LZ (fmap f left) (f el) (fmap f right)

shiftLeft :: ListZipper a -> ListZipper a
shiftLeft (LZ (lh:lt) el r) = LZ lt lh (el : r)
shiftLeft _                 = error "leftDestructure"

shiftRight :: ListZipper a -> ListZipper a
shiftRight (LZ l el (rh:rt)) = LZ (el : l) rh rt
shiftRight _                 = error "rightDestructure"

apply :: (a -> a) -> a -> [a]
apply f = tail . iterate f

shift :: (a -> a) -> (a -> a) -> a -> ListZipper a
shift f g e = LZ (apply f e) e (apply g e)

instance Comonad ListZipper where
  duplicate = shift shiftLeft shiftRight
  extract (LZ _ el _) = el

insert :: a -> ListZipper a -> ListZipper a
insert el (LZ left _ right) = LZ left el right

truncateLZ :: Int -> ListZipper a -> [a]
truncateLZ cnt (LZ left el right) =
  reverse (take cnt left) ++ [el] ++ take cnt right
