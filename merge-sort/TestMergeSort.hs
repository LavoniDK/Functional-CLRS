module TestMergeSort where

import MergeSort (mergeSort, merge)

testAssert :: (Ord a, Eq a, Show a) => String -> [a] -> [a] -> IO ()
testAssert testName input expected =
    let result = mergeSort input 0 (length input - 1)
    in if result == expected
        then putStrLn $ "[PASS] " ++ testName
        else putStrLn $
            "[FAIL] " ++ testName
            ++ "\n  Input: " ++ show input
            ++ "\n  Expected: " ++ show expected
            ++ "\n  Got: " ++ show result
main :: IO ()
main = do
    putStrLn "Running MergeSort tests..."
    -- Test cases
    testAssert "Unsorted list" [5, 3, 8, 1, 2] [1, 2, 3, 5, 8]
    testAssert "Sorted list" [1, 2, 3, 5, 8] [1, 2, 3, 5, 8]
    testAssert "List with duplicates" [5, 3, 8, 3, 2, 1, 5] [1, 2, 3, 3, 5, 5, 8]
    testAssert "Empty list" ([] :: [Int]) []
    testAssert "List with one element" [7] [7]
    putStrLn "All tests completed."