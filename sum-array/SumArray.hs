module SumArray (sumArray, sumArrayFold) where

sumArray :: (Num a) => [a] -> a 
sumArray a = sumArray' a 0
    where sumArray' [] acc     = acc
          sumArray' (x:xs) acc = sumArray' xs (x + acc)

-- or simply
sumArrayFold :: (Num a) => [a] -> a 
sumArrayFold = foldl (+) 0