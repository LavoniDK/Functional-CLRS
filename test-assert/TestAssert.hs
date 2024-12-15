module Test<Module> where

import <Module> (<module>)
testAssert :: (Ord a, Eq a, Show a) => String -> [a] -> [a] -> IO ()
testAssert testName input expected =
    let result = <module> input
    in if result == expected
        then putStrLn $ "[PASS] " ++ testName
        else putStrLn $ "[FAIL] " ++ testName ++ "\n  Input: " ++ show input ++ "\n  
                                                      Expected: " ++ show expected ++ "\n  
                                                      Got: " ++ show result

main :: IO ()
main = do
    putStrLn "Running <module> tests..."
    -- Test cases
    putStrLn "All tests completed."
