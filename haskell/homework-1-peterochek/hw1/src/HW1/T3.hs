module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (s, _) _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf                  = -1
tdepth (Branch (_, h) _ _ _) = h

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ left y right)
    | x < y     = tmember x left
    | x > y     = tmember x right
    | otherwise = True


tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left x right = Branch (tsize left + tsize right + 1, 1 + max (tdepth left) (tdepth right)) left x right


tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x (Branch m left y right)
    | x < y = rebalance $ mkBranch newLeft y right
    | x > y = rebalance $ mkBranch left y newRight
    | otherwise = Branch m left y right -- duplicate
    where newLeft = tinsert x left
          newRight = tinsert x right


rebalance :: Tree a -> Tree a
rebalance Leaf = Leaf
rebalance t@(Branch _ left v right)
  | diff <= -2 = rightRotate (mkBranch newLeft v right)
  | diff >= 2 = leftRotate (mkBranch left v newRight)
  | otherwise = t
    where
      diff = balance t
      newLeft = if balance left > 0 then leftRotate left else left
      newRight = if balance right < 0 then rightRotate right else right

balance :: Tree a -> Int
balance Leaf                   = 0
balance (Branch _ Leaf _ Leaf) = 0
balance (Branch _ l _ r)       = tdepth r - tdepth l

leftRotate :: Tree a -> Tree a
leftRotate (Branch _ left y (Branch _ nestedLeft v nestedRight)) = mkBranch newLeft v nestedRight
    where newLeft = mkBranch left y nestedLeft
leftRotate _ = error "impossible"

rightRotate :: Tree a -> Tree a
rightRotate (Branch _ (Branch _ nestedLeft v nestedRight) y right) = mkBranch nestedLeft v newRight
  where newRight = mkBranch nestedRight y right
rightRotate _ = error "impossible"
