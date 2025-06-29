module Test<Module> where

import <Module> (<module1>, <module2>)

-- General test runner for a given <Module>‐evaluation function
testAssertWith :: (Eq a, Show a, Num a) => String -> ([a] -> a -> a) -> [a] -> a -> a -> IO ()
testAssertWith testName <Module>Fn coeffs x expected =
    let result = <Module>Fn coeffs x
    in if result == expected
          then putStrLn $ "[PASS] " ++ testName
          else putStrLn $
               "[FAIL] " ++ testName
               ++ "\n  Coeffs: "   ++ show coeffs
               ++ "\n  x: "        ++ show x
               ++ "\n  Expected: " ++ show expected
               ++ "\n  Got: "      ++ show result

main :: IO ()
main = do
    putStrLn "Running <Module> tests..."
    let test f label = testAssertWith (label ++ " — " ++ f)
                                       (if f == "<module1>" then <module1> else <module2>)

    -- Runs both <module1> and <module2> on the same inputs
    let runAll label coeffs x expected = do
            test "<module1>"     label coeffs x expected
            test "<module2>" label coeffs x expected

    -- Test cases (use runAll)

    putStrLn "All tests completed."
