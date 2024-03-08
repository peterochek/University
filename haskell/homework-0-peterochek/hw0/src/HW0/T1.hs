{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso leftAssoc rightAssoc
  where
    leftAssoc (a, (b, c)) = ((a, b), c)
    rightAssoc ((a, b), c) = (a, (b, c))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftAssoc rightAssoc
  where
    leftAssoc (Left a)          = Left (Left a)
    leftAssoc (Right (Left b))  = Left (Right b)
    leftAssoc (Right (Right c)) = Right c

    rightAssoc (Left (Left a))  = Left a
    rightAssoc (Left (Right b)) = Right (Left b)
    rightAssoc (Right c)        = Right (Right c)
