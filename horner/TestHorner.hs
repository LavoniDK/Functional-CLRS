module TestHorner where

import Horner (horner, hornerFold)

-- General test runner for a given Horner‐evaluation function
testAssertWith :: (Eq a, Show a, Num a) => String -> ([a] -> a -> a) -> [a] -> a -> a -> IO ()
testAssertWith testName hornerFn coeffs x expected =
    let result = hornerFn coeffs x
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
    putStrLn "Running Horner tests..."
    let test f label = testAssertWith (label ++ " — " ++ f)
                                       (if f == "horner" then horner else hornerFold)

    -- Runs both horner and hornerFold on the same inputs
    let runAll label coeffs x expected = do
            test "horner"     label coeffs x expected
            test "hornerFold" label coeffs x expected

    -- Test cases

    -- Empty coefficient list should always yield 0
    runAll "Empty coeffs, x=0"     []    0    0
    runAll "Empty coeffs, x=10"    []   10    0

    -- Single‐term polynomial p(x) = 5
    runAll "Constant = 5, x=0"     [5]    0    5
    runAll "Constant = 5, x=100"   [5]  100    5

    -- Linear polynomial p(x) = 3 + 4x
    -- coeffs = [a0, a1] = [3,4]
    runAll "Linear 3+4x, x=0"      [3,4]  0    3
    runAll "Linear 3+4x, x=2"      [3,4]  2   11   -- 3 + 4*2 = 11

    -- Quadratic p(x) = 5 + 3x + 2x^2
    -- coeffs = [5,3,2]
    runAll "Quadratic 5+3x+2x^2, x=0"  [5,3,2]  0   5
    runAll "Quadratic 5+3x+2x^2, x=1"  [5,3,2]  1  10   -- 5 + 3*1 + 2*1^2 = 10
    runAll "Quadratic 5+3x+2x^2, x=10" [5,3,2] 10 235   -- 5 + 3*10 + 2*100 = 235

    -- Higher‐degree: p(x) = 1 - x + x^3  (coeffs [1, -1, 0, 1])
    runAll "Cubic 1 - x + x^3, x=0"   [1,-1,0,1]  0    1
    runAll "Cubic 1 - x + x^3, x=1"   [1,-1,0,1]  1    1   -- 1 -1 +0 +1 = 1
    runAll "Cubic 1 - x + x^3, x=2"   [1,-1,0,1]  2    7   -- 1 -2 + 0 + 8 = 7

    -- Negative and zero coefficients: p(x) = -2 + 0*x + 4x^2
    runAll "Quadratic -2+0x+4x^2, x=3"  [-2,0,4]  3   34   -- -2 + 0 + 4*9 = 34

    -- Float coefficients
    runAll "Floats: [1.5, -2.5, 4.0], x=0" [1.5, -2.5, 4.0]  0    1.5
    runAll "Floats: [1.5, -2.5, 4.0], x=1" [1.5, -2.5, 4.0]  1    3.0  -- 1.5 - 2.5 + 4 = 3.0
    runAll "Floats: [1.5, -2.5, 4.0], x=2" [1.5, -2.5, 4.0]  2   12.5  -- 1.5 -5 + 16 = 12.5

    putStrLn "All tests completed."
