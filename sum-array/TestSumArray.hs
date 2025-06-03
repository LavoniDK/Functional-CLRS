module TestSumArray where
import SumArray (sumArray, sumArrayFold)

-- General test runner for a given summing function
testAssertWith :: (Eq a, Show a, Num a) => String -> ([a] -> a) -> [a] -> a -> IO ()
testAssertWith testName sumFn input expected =
    let result = sumFn input
    in if result == expected
          then putStrLn $ "[PASS] " ++ testName
          else putStrLn $
               "[FAIL] " ++ testName
               ++ "\n  Input: " ++ show input
               ++ "\n  Expected: " ++ show expected
               ++ "\n  Got: " ++ show result

main :: IO ()
main = do
    putStrLn "Running sumArray tests..."
    let test f label = testAssertWith (label ++ " — " ++ f) (if f == "sumArray" then sumArray else sumArrayFold)

    -- Define a helper that runs all tests for both implementations
    let runAll label input expected = do
            test "sumArray" label input expected
            test "sumArrayFold" label input expected

    -- Test cases
    runAll "Empty list"          ([]      :: [Int])     0
    runAll "Single positive"     ([42]    :: [Int])    42
    runAll "Single negative"     ([-7]    :: [Int])    (-7)
    runAll "All positives"       ([1,2,3] :: [Int])      6
    runAll "All negatives"       ([-1,-2,-3] :: [Int])  (-6)
    runAll "Mixed positives/neg" ([10, -3, 5, -2] :: [Int]) 10
    runAll "Zeros and positives" ([0,0,5,0] :: [Int])    5

    -- Floating-point tests
    runAll "Floats: empty"       ([]      :: [Double])   0.0
    runAll "Floats: single"      ([3.14]  :: [Double])   3.14
    runAll "Floats: mixed"       ([1.5, -2.5, 4.0] :: [Double]) 3.0

    putStrLn "All tests completed."
