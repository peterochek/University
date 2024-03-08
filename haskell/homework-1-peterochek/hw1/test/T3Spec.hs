module T3Spec where

import Test.Hspec
import Data.List (sort)
import System.Random


import HW1.T3

branchDepths :: Tree a -> [Int]
branchDepths tree = go tree 0
  where
    go Leaf _ = []  -- Reached a leaf, no depth to record
    go (Branch _ left _ right) depth =
      depth : (go left (depth + 1) ++ go right (depth + 1))

branchSizes :: Tree a -> [Int]
branchSizes = go
  where
    go Leaf = []  -- Reached a leaf, no size to record
    go (Branch (size, _) left _ right) =
      size : (go left ++ go right)

inOrderTraversal :: Tree a -> [a]
inOrderTraversal Leaf = []
inOrderTraversal (Branch _ left x right) =
      inOrderTraversal left ++ [x] ++ inOrderTraversal right

isBST :: Ord a => Tree a -> Bool
isBST tree = isSorted (inOrderTraversal tree)
  where
    isSorted :: Ord a => [a] -> Bool
    isSorted []             = True
    isSorted [_]            = True
    isSorted (x1 : x2 : xs) = x1 <= x2 && isSorted (x2 : xs)

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Branch _ left _ right) =
  let
    leftHeight = tdepth left
    rightHeight = tdepth right
  in
    abs (leftHeight - rightHeight) <= 1 && isBalanced left && isBalanced right

isTreeGood :: Ord a => Tree a -> Bool
isTreeGood tree = isBalanced tree && isBST tree && (inOrderTraversal tree == sort (inOrderTraversal tree))


spec :: Spec
spec = do
  describe "tFromList" $
    it "build a Tree from given List of elements" $ do
        let n = 1000
        let list = [1..n]
        let tree = tFromList list
        tsize tree `shouldBe` n

        let allMembers = all (`tmember` tree) list
        allMembers `shouldBe` True

        let contains1000 = tmember 1000 tree
        let contains1001 = tmember 1001 tree

        contains1000 `shouldBe` True
        contains1001 `shouldBe` False

  describe "0, -2, -1" $
    it "check small" $ do
        let list = [0, -2, -1]
        let tree = tFromList list

        let checkTree = isTreeGood tree
        checkTree `shouldBe` True
        -- depths `shouldBe` True

  describe "1..700" $
    it "check big" $ do
        let list = [1..700]
        let tree = tFromList list

        let traversed = inOrderTraversal tree
        let isTreeBST = isBST tree
        let isTreeBalanced = isBalanced tree

        let depths = branchDepths tree

        traversed `shouldBe` sort list
        isTreeBST `shouldBe` True
        isTreeBalanced `shouldBe` True
        -- depths `shouldBe` True

  describe "max balanced" $
    it "check big" $ do
        let maxSizes = [1..50]
        let lists = map (\maxSize -> [1..maxSize]) maxSizes
        let trees = map tFromList lists

        let goodTrees = map isTreeGood trees

        let allGood = and goodTrees
        allGood `shouldBe` True
