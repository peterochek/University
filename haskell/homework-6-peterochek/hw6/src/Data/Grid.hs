module Data.Grid
  ( Grid(..)
  , insertGrid
  , shifts
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), insert, shift, shiftLeft, shiftRight)

newtype Grid a =
  Grid
    { unGrid :: ListZipper (ListZipper a)
    }

instance Functor Grid where
  fmap f g = Grid (fmap f <$> unGrid g)

up :: Grid a -> Grid a
up (Grid g) = Grid (shiftLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (shiftRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap shiftLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap shiftRight g)

shifts :: [Grid a -> Grid a]
shifts = [left, up, right, down]

insertGrid :: a -> Grid a -> Grid a
insertGrid el (Grid g) = Grid (insert cur g)
  where
    cur = insert el (extract g)

row :: Grid a -> ListZipper (Grid a)
row = shift left right

column :: Grid a -> ListZipper (Grid a)
column = shift up down

instance Comonad Grid where
  duplicate = Grid . fmap row . column
  extract (Grid g) = extract (extract g)
