myLength :: [Int] -> Int
myLength []       = 0
myLength (x : xs) = 1+myLength xs

myMaximum :: [Int] -> Int
myMaximum2 :: [Int] -> Int -> Int
myMaximum2 [] n = n
myMaximum2 (x : xs) n    | x > n = myMaximum2 xs x
                        | True  = myMaximum2 xs n
myMaximum (x : xs) = myMaximum2 xs x

average :: [Int] -> Float
sumL :: [Int] -> Int -> Int
sumL [] n       = n
sumL (x : xs) n = n+sumL xs x
average (x : xs) = fromIntegral (div (fromIntegral (sumL xs x)) (fromIntegral (1+length xs)))

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = palindrome x []
    where
        palindrome [] y     = y
        palindrome (x:xs) y = palindrome xs (x : (y ++ [x]))

remove :: [Int] -> [Int] -> [Int]
search (x:xs) n | n == x = True
                | otherwise = search xs n
search [] n = False
remove (x:xs) y     | search y x = remove xs y
                    | otherwise = x : remove xs y
remove [] y =  []

flatten :: [[Int]] -> [Int]
flatten (x:xs) = x ++ (flatten xs)
flatten []     = []


oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens (x:xs)   | (mod x 2 == 0)    = (o, x:e)
                    | otherwise         = (x:o, e)
                    where
                        (o, e) = oddsNevens xs
oddsNevens [] = ([], [])

primeDivisors :: Int -> [Int]
firstDivisor :: Int -> Int -> Int
firstDivisor n m    | m*m > n = n
                    | (mod n m == 0) = m
                    | otherwise = firstDivisor n (m+1)

primeDivisors n | (fd == 1 || fd == 0) = []
                | (fd == n) = [n]
                | otherwise = fd : (primeDivisors (divdiv n fd))
                    where
                        fd = firstDivisor n 2
                        divdiv n d  | (mod n d == 0) = divdiv (div n d) d
                                    | otherwise = n
