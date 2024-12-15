module QuickSort (quickSort) where

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (p:xs) = quickSort lo ++ [p] ++ quickSort hi
    where
        lo = filter (< p) xs
        hi = filter (>= p) xs