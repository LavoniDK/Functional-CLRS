module InsertionSort (insertionSort, insert) where
    
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x sorted@(y:ys)
    | x <= y    = x : sorted
    | otherwise = y : insert x ys