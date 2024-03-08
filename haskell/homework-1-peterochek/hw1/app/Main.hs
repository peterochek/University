import HW1.T3

branchDepths :: Tree a -> [(a, Int)]
branchDepths tree = go tree
  where
    go Leaf = []  -- Reached a leaf, no size to record
    go (Branch (_, depth) left v right) =
      (v, depth) : (go left ++ go right)

branchSizes :: Tree a -> [(a, Int)]
branchSizes tree = go tree
  where
    go Leaf = []  -- Reached a leaf, no size to record
    go (Branch (size, _) left v right) =
      (v, size) : (go left ++ go right)

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

size :: Tree a -> Int
size Leaf    = 0
size (Branch _ l _ r) = size l + size r + 1

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Branch _ l _ r) = 
  isBalanced l && isBalanced r && abs (tdepth l - tdepth r) <= 1


main :: IO ()
main = do
  let list = [0, -1, -2]
  let tree = tFromList list

  let traversed = inOrderTraversal tree
  let isTreeBST = isBST tree
  let isTreeBalanced = isBalanced tree

  print traversed
  print isTreeBST
  print isTreeBalanced
  print tree
  print (branchSizes tree)
  print (branchDepths tree)

