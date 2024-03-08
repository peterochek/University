module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , extend
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P w x, P y z) = P (w, y) (x, z)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a1, Success a2) = Success (a1, a2)
distExcept (Error e, _)             = Error e
distExcept (_, Error e)             = Error e

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a1 :> rest1, a2 :> rest2) = (a1, a2) :> distStream (rest1, rest2)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

extend :: List a -> List a -> List a
extend Nil a       = a
extend (x :. xs) a = x :. extend xs a

distList :: (List a, List b) -> List (a, b)
distList (Nil, _)     = Nil
distList (_, Nil)     = Nil
distList (x :. xs, y) = extend (mapList (\b -> (x, b)) y) (distList (xs, y)) 

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\i -> (f1 i, f2 i))

wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)
