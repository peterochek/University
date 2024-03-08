{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
    Contains x '[]       = 'False
    Contains x (x ': xs) = 'True
    Contains x (y ': xs) = Contains x xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
    Delete x '[]       = '[]
    Delete x (x ': xs) = xs
    Delete x (y ': xs) = y ': Delete x xs

type family Add (v :: Symbol) (set :: TSet) :: TSet where
    Add x '[]       = '[x]
    Add x (x ': xs) = x ': xs
    Add x (y ': xs) = y ': Add x xs

