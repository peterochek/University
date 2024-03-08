module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat []             = mempty
mcat (Just x : xs)  = x <> mcat xs
mcat (Nothing : xs) = mcat xs

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart []             = (mempty, mempty)
epart (Left a : xs)  = let (as, bs) = epart xs in (a <> as, bs)
epart (Right b : xs) = let (as, bs) = epart xs in (as, b <> bs)
