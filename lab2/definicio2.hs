countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = length (filter f l)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = foldr (mapf xs) [] fs
    where mapf xs f acc = (map f xs):acc

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = foldr (mapx fs) [] xs
    where mapx fs x acc = (invMap fs x):acc
            where   invMap fs x = [f x | f <- fs]

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl ff f ini l = foldl f ini (filter ff l)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] n = [n]
insert f (x : xs) n
    | f x n = x : (insert f xs n)
    | otherwise = n : x : xs

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f xs = foldl (insert f) [] xs
