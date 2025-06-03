module BubbleSort (bubbleSort) where

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubble xs 0 (length xs - 1)
  where
    bubble :: (Ord a) => [a] -> Int -> Int -> [a]
    bubble x i j
      | i >= length x - 1  = x
      | j <= i             = bubble x (i + 1) (length x - 1)
      | otherwise =
          let a = x !! j
              b = x !! (j - 1)
              x'= if a < b
                    then (take (j - 1) x) ++ [a, b] ++ (drop (j + 1) x)
                    else x
          in bubble x' i (j - 1)
