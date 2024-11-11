module MergeSort (mergeSort, merge) where

mergeSort :: [Int] -> Int -> Int -> [Int]
mergeSort xs lo hi
    | lo >= hi  = [xs !! lo]  
    | otherwise = merge (mergeSort xs lo mid) (mergeSort xs (mid + 1) hi)
  where mid = div (lo + hi) 2

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys                   
merge xs [] = xs 
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
