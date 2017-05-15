insert :: [Int] -> Int -> [Int]
insert (x : xs) n
    | x >= n = n : x : xs
    | otherwise = x : (insert xs n)
insert [] n = [n]

isort :: [Int] -> [Int]
isort (x : xs) = insert (isort xs) x
isort [] = []

remove :: [Int] -> Int -> [Int]
remove (x : xs) n
    | x == n = xs
    | otherwise = x : (remove xs n)
remove [] n = []

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = m : (ssort (remove l m))
    where
        m = fmin l
        where
            fmin (x:xs)
                | xs == [] = x
                | otherwise = min x (fmin xs)

merge :: [Int] -> [Int] -> [Int]
merge (x : xs) (y : ys)
    | x > y     = y : (merge (x:xs) ys)
    | otherwise = x : (merge xs (y:ys))
merge [] l = l
merge l [] = l

splitSize (x : xs) s
    | (s == 1) = ([x], xs)
    | (xs == []) = ([], (x : xs))
    | otherwise = (x:s1, s2)
        where (s1, s2) = splitSize xs (s-1)
splitSize [] _ = ([],[])

msort :: [Int] -> [Int]
msort [] = []
msort l
    | length l == 1 = l
    | otherwise = merge (msort l1) (msort l2)
    where
        (l1, l2) = splitSize l (div (length l) 2)

pivot (x:xs) p
    | x < p = (x:lt, rg)
    | otherwise = (lt, x:rg)
    where
        (lt, rg) = pivot xs p
pivot [] _ = ([], [])

qsort :: [Int] -> [Int]
qsort (x : xs) = (qsort left) ++ (x : (qsort right))
    where (left, right) = pivot xs x
qsort [] = []

genQsort :: Ord a => [a] -> [a]
genQsort (x : xs) = (genQsort left) ++ (x : (genQsort right))
    where (left, right) = pivot xs x
genQsort [] = []
