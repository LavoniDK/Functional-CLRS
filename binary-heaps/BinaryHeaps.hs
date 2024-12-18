module BinaryHeaps (parent, left, right, maxHeapify) where
    
parent :: Int -> Int
parent i = div i 2

left :: Int -> Int
left i = 2 * i  

right :: Int -> Int
right i = 2 * i + 1

swap :: [a] -> Int -> Int -> [a]
swap xs i j = let a = xs !! i
                  b = xs !! j
              in take i xs ++ [b] ++ drop (i + 1) (take j xs) ++ [a] ++ drop (j + 1) xs

maxHeapify :: [Int] -> Int -> [Int]
maxHeapify xs i
    | largest /= i = maxHeapify (swap xs i largest) largest
    | otherwise = xs
  where
    n = length xs
    l = left i
    r = right i
    largest
        | l < n && xs !! l > xs !! i = 
            if r < n && xs !! r > xs !! l then r else l
        | r < n && xs !! r > xs !! i = r
        | otherwise = i

buildMaxHeap :: [Int] -> [Int]
buildMaxHeap [] = []
buildMaxHeap xs = 
    buildMaxHeap init heap ++ last heap
        where mid = div (length xs) 2
              heap = maxHeapify xs mid


main :: IO ()
main = do
    let heap = [3, 2, 1, 7, 8, 4, 10, 16, 12]
    print $ maxHeapify heap 0