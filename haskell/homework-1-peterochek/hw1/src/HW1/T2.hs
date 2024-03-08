module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z n     = n
nplus (S m) n = S (nplus m n)

nmult :: N -> N -> N
nmult Z _     = Z
nmult (S m) n = nplus n (nmult m n)

nsub :: N -> N -> Maybe N
nsub m Z         = Just m
nsub Z _         = Nothing
nsub (S m) (S n) = nsub m n

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S m) (S n) = ncmp m n

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (pred n))

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = nToNum n + 1

nEven :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S n)) = nEven n

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = error "Division by zero"
ndiv Z _ = Z
ndiv m n = case nsub m n of
  Nothing  -> Z
  Just Z   -> S Z
  Just myRem -> S (ndiv myRem n)

nmod :: N -> N -> N
nmod m n = case nsub m n of
  Nothing  -> m
  Just Z   -> Z
  Just myRem -> nmod myRem n
