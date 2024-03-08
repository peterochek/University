module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last a <> ys    = a :+ ys
  (x :+ xs) <> ys = x :+ (xs <> ys)

data Inclusive a b = This a | That b | Both a b -- 3 * 3 = 9 variants
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This w <> This x     = This (w <> x)
  This w <> That x     = Both w x
  This w <> Both x y   = Both (w <> x) y
  That w <> This x     = Both x w
  That w <> That x     = That (w <> x)
  That w <> Both x y   = Both x (w <> y)
  Both w x <> This y   = Both (w <> y) x
  Both w x <> That y   = Both w (x <> y)
  Both w x <> Both y z = Both (w <> y) (x <> z)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS s1 <> DS "" = DS s1
  DS "" <> DS s2 = DS s2
  DS s1 <> DS s2 = DS (s1 <> "." <> s2)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
