module TestBinaryHeaps where
import BinaryHeaps (
    Tree(..), 
    buildMaxHeap,
    heapSort,
    maxHeapInsert,
    maxHeapExtractMax,
    maxHeapIncreaseKey,
    heapSize,
    listToTree,
    treeToList) 

maxHeapMax :: Tree a -> a
maxHeapMax Nil = error "Empty heap has no maximum"
maxHeapMax (Node c _ _) = c

testAssert :: (Eq a, Show a) => String -> a -> a -> IO ()
testAssert testName result expected =
    if result == expected
        then putStrLn $ "[PASS] " ++ testName
        else putStrLn $
            "[FAIL] " ++ testName
            ++ "\n  Input: " ++ show input
            ++ "\n  Expected: " ++ show expected
            ++ "\n  Got: " ++ show result

main :: IO ()
main = do
    putStrLn "Running BinaryHeaps tests..."

    -- buildMaxHeap (visually checked)
    let tree = listToTree [3, 5, 1, 10, 2]
    let built = buildMaxHeap tree
    testAssert "buildMaxHeap: max element at root" (maxHeapMax built) 10

    -- heapSort
    testAssert "heapSort: sort ascending" (heapSort [3, 5, 1, 10, 2]) [10, 5, 3, 2, 1]
    testAssert "heapSort: already sorted" (heapSort [9, 8, 7]) [9, 8, 7]
    testAssert "heapSort: reversed input" (heapSort [1, 2, 3]) [3, 2, 1]

    -- maxHeapInsert
    let tree1 = buildMaxHeap (listToTree ([4, 2, 3] :: [Int]))
    let inserted = maxHeapInsert tree1 (10 :: Int)
    testAssert "maxHeapInsert: 10 becomes new root" (maxHeapMax inserted) 10

    -- maxHeapExtractMax
    let tree2 = buildMaxHeap (listToTree [8, 3, 6, 1, 4])
    let (max1, after1) = maxHeapExtractMax tree2
    testAssert "maxHeapExtractMax: returns max" max1 8
    testAssert "maxHeapExtractMax: second max is next root" (maxHeapMax after1) 6

    -- maxHeapIncreaseKey
    let tree3 = buildMaxHeap (listToTree [4, 2, 3])
    let increased = maxHeapIncreaseKey tree3 3 9  -- assume position 3 is leaf with value 3
    testAssert "maxHeapIncreaseKey: bubbling up to root" (maxHeapMax increased) 9

    putStrLn "All tests completed."