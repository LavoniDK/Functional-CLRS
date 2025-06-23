module TestStrassen where

import Strassen (SqMatrix(..), matrixMul, matrixMul', matrixMul'', strassen)

-- General test runner for a given Strassen‐evaluation function
testAssertWith 
  :: (Eq a, Show a, Num a)
  => String 
  -> (SqMatrix a -> SqMatrix a -> SqMatrix a) 
  -> SqMatrix a 
  -> SqMatrix a 
  -> SqMatrix a 
  -> IO ()
testAssertWith testName fn m1 m2 expected =
    let result = fn m1 m2
    in if result == expected
          then putStrLn $ "[PASS] " ++ testName
          else putStrLn $
               "[FAIL] " ++ testName
               ++ "\n  m1:      " ++ show m1
               ++ "\n  m2:      " ++ show m2
               ++ "\n  Expected:" ++ show expected
               ++ "\n  Got:     " ++ show result

main :: IO ()
main = do
    putStrLn "Running Strassen tests..."
    let test f label = testAssertWith (label ++ " — " ++ f)
                                       (if f == "matrixMul" then matrixMul else 
                                            if f == "matrixMul'" then matrixMul' else strassen)

    -- Runs both matrixMul and strassen on the same inputs
    let runAll label m1 m2 expected = do
            test "matrixMul" label m1 m2 expected
            test "matrixMul'" label m1 m2 expected
            test "strassen" label m1 m2 expected

    -- Test cases
    runAll "1×1 multiplication"
        (Scalar 2) (Scalar 3)
        (Scalar 6)

    runAll "2×2 identity"
        (Block (Scalar 1) (Scalar 0)
               (Scalar 0) (Scalar 1))
        (Block (Scalar 5) (Scalar 6)
               (Scalar 7) (Scalar 8))
        (Block (Scalar 5) (Scalar 6)
               (Scalar 7) (Scalar 8))

    runAll "2×2 general"
        (Block (Scalar 1) (Scalar 2)
               (Scalar 3) (Scalar 4))
        (Block (Scalar 5) (Scalar 6)
               (Scalar 7) (Scalar 8))
        (Block (Scalar 19) (Scalar 22)
               (Scalar 43) (Scalar 50))

    putStrLn "All tests completed."
