module BinaryHeaps (
    Tree(..), 
    buildMaxHeap,
    heapSort,
    maxHeapInsert,
    maxHeapExtractMax,
    maxHeapIncreaseKey, 
    heapSize,
    listToTree,
    treeToList
) where

data Tree a = Nil | Node a (Tree a) (Tree a) 
    deriving (Show)

maxHeapify :: (Ord a) => Tree a -> Tree a
maxHeapify Nil = Nil
maxHeapify t@(Node c Nil Nil) = t
maxHeapify t@(Node c (Node lc ll lr) Nil)
    | lc > c    = Node lc (maxHeapify (Node c ll lr)) Nil
    | otherwise = t
maxHeapify t@(Node c l@(Node lc ll lr) r@(Node rc rl rr))
    | lc > c, lc >= rc = Node lc (maxHeapify (Node c ll lr)) r
    | rc > c, rc >= lc = Node rc l (maxHeapify (Node c rl rr))
    | otherwise = t

buildMaxHeap :: (Ord a) => Tree a -> Tree a
buildMaxHeap t@(Node c Nil _) = t
buildMaxHeap (Node c l r) = maxHeapify (Node c (buildMaxHeap l) (buildMaxHeap r))

heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs = extractAll $ buildMaxHeap $ listToTree xs
  where
    extractAll Nil = []
    extractAll t   = let (maxVal, newHeap) = maxHeapExtractMax t
                     in maxVal : extractAll newHeap

-- Due to list-tree transformation this function is O(n), not O(log n) - i am missing the smart recursive solution
maxHeapExtractMax :: (Ord a) => Tree a -> (a, Tree a)
maxHeapExtractMax (Node c Nil _) = (c, Nil)
maxHeapExtractMax (Node c (Node lc _ _) Nil) = (c, Node lc Nil Nil)
maxHeapExtractMax t@(Node c l r) = (c, maxHeapify $ listToTree $ swapNdrop $ treeToList t)
  where
    swapNdrop xs = last xs : tail (init xs)

maxHeapIncreaseKey :: (Ord a) => Tree a -> Int -> a -> Tree a  
maxHeapIncreaseKey (Node c l r) 1 k
  | k < c     = error "new key is smaller than current key"
  | otherwise = Node k l r
maxHeapIncreaseKey (Node c l r) i k
  | even i = bubbleFromLeft (Node c (maxHeapIncreaseKey l (div i 2) k) r)
  | otherwise = bubbleFromRight (Node c l (maxHeapIncreaseKey r (div i 2) k))
    where 
      bubbleFromLeft (Node p l@(Node lc ll lr) r)
        | lc > p    = Node lc (Node p ll lr) r
        | otherwise = Node p l r
      bubbleFromRight (Node p l r@(Node rc rl rr))
        | rc > p    = Node rc l (Node p rl rr)
        | otherwise = Node p l r

maxHeapInsert :: (Ord a, Bounded a) => Tree a -> a -> Tree a
maxHeapInsert Nil k = Node k Nil Nil 
maxHeapInsert t@(Node c l r) k
  | heapSize l <= heapSize r = maxHeapIncreaseKey (Node c (maxHeapInsert' l) r) (heapSize t + 1) k
  | otherwise                = maxHeapIncreaseKey (Node c l (maxHeapInsert' r)) (heapSize t + 1) k
  where 
    size = heapSize t + 1

maxHeapInsert' :: (Ord a, Bounded a) => Tree a -> Tree a  
maxHeapInsert' (Node c Nil Nil) = Node c (Node minBound Nil Nil) Nil 
maxHeapInsert' (Node c l Nil) = Node c l (Node minBound Nil Nil)
maxHeapInsert' (Node c l r)
  | heapSize l <= heapSize r = (Node c (maxHeapInsert' l) r) 
  | otherwise                = (Node c l (maxHeapInsert' r))

-- Auxilliary functions
heapSize :: Tree a -> Int
heapSize Nil = 0
heapSize (Node c l r) = 1 + heapSize l + heapSize r

treeToList :: Tree a -> [a]
treeToList t = bfs [t]
  where
    bfs [] = []
    bfs (Nil : xs) = bfs xs
    bfs (Node c l r : xs) = c : bfs (xs ++ [l, r])

listToTree :: (Ord a) => [a] -> Tree a
listToTree xs = build 0
  where
    n = length xs
    build i
      | i >= n      = Nil
      | 2*i + 1 > n = Node (xs !! i) Nil Nil
      | 2*i + 2 > n = Node (xs !! i) (build (2*i + 1)) Nil
      | otherwise   = Node (xs !! i) (build (2*i + 1)) (build (2*i + 2))