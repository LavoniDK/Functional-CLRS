module TestInsertionSort where
import InsertionSort (insertionSort, insert)

testAssert :: (Ord a, Eq a, Show a) => String -> [a] -> [a] -> IO ()
testAssert testName input expected =
    let result = insertionSort input
    in if result == expected
        then putStrLn $ "[PASS] " ++ testName
        else putStrLn $
            "[FAIL] " ++ testName
            ++ "\n  Input: " ++ show input
            ++ "\n  Expected: " ++ show expected
            ++ "\n  Got: " ++ show result

main :: IO ()
main = do
    putStrLn "Running insertion sort tests..."
    -- Test cases
    testAssert "Empty list" ([] :: [Int]) []
    testAssert "Single element" [42] [42]
    testAssert "Sorted list" [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]
    testAssert "Reverse sorted list" [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]
    testAssert "Unsorted list" [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5] [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
    testAssert "List with duplicates" [4, 2, 4, 3, 4] [2, 3, 4, 4, 4]
    testAssert "List with negative numbers" [3, -1, 4, -2, 0] [-2, -1, 0, 3, 4]
    putStrLn "All tests completed."