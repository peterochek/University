module HW2.T2
  ( joinWith
  , splitOn
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim = foldr nested ([] :| [])
  where
    nested x (xs :| xss)
      | x == delim = [] :| (xs : xss)
      | otherwise  = (x : xs) :| xss

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = foldr1 (\a b -> a ++ [sep] ++ b) (x:xs)
