module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1
import HW3.T2 (extend)

joinOption :: Option (Option a) -> Option a
joinOption None         = None
joinOption (Some inner) = inner

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)       = Error e
joinExcept (Success inner) = inner

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# y) :# z) = x :# (z <> y)

joinList :: List (List a) -> List a
joinList Nil           = Nil
joinList (inner :. xs) = extend inner (joinList xs)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F g1) = F (\x -> let (F g2) = g1 x in g2 x)
