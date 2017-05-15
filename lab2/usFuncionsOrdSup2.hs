flatten :: [[Int]] -> [Int]
flatten = foldr (++) []

myLength :: String -> Int
myLength = foldr (count) 0
    where count _ acc  = acc + 1

myReverse :: [Int] -> [Int]
myReverse = foldl (rev) []
    where rev acc v = v : acc

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (\ll -> length (filter (==x) ll)) l

firstWord :: String -> String
firstWord l = takeWhile (/= ' ') (dropWhile (== ' ') l)
