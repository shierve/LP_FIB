absValue :: Int -> Int
absValue n  | n >= 0    = n
            | otherwise = -n

power :: Int -> Int -> Int
power x p   | p == 0    = 1
            | otherwise = x * power (x) (p-1)

hasFactorsAfter :: Int -> Int -> Bool
hasFactorsAfter n m     | m*m > n                   = False
                        | mod n m == 0              = True
                        | otherwise                 = hasFactorsAfter n (m+2)

isPrime :: Int -> Bool
isPrime n   | (n < 2)       = False
            | (n == 2)      = True
            | mod n 2 == 0  = False
            | otherwise     = not (hasFactorsAfter n 3)

slowFib :: Int -> Int
slowFib n   | n == 0    = 0
            | n == 1    = 1
            | otherwise = (slowFib (n-1))+(slowFib (n-2))

fibFinestra :: Int -> (Int, Int)
fibFinestra n   | n == 0    = (0, 1)
                | n == 1    = (1, 1)
                | otherwise = (f2, f1+f2)
                where
                    (f1, f2) = fibFinestra (n-1)

quickFib :: Int -> Int
quickFib n  = f1 where (f1, f2) = fibFinestra (n)
