sumMultiples35 :: Integer -> Integer
sumMultiples35 x = (sumMultiplesi 3) + (sumMultiplesi 5) - (sumMultiplesi 15)
    where
        sumInt n = div (n*(n+1)) 2
        sumMultiplesi i = i*(sumInt $ (div (x-1) i) )

fib = 0 : (1 : (zipWith (+) fib (tail fib)))

fibonacci :: Int -> Integer
fibonacci n = fib !! n

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum (filter even (takeWhile (<n) fib))

largestPrimeFactor :: Int -> Int
firstDivisor n m    | m*m > n = n
                    | (mod n m == 0) = m
                    | otherwise = firstDivisor n (m+1)

largestPrimeFactor n
                | (fd == n) = n
                | fd > lpf = fd
                | otherwise = lpf
                where
                    fd = firstDivisor n 2
                    lpf = largestPrimeFactor (divdiv n fd)
divdiv n d  | (mod n d == 0) = divdiv (div n d) d
            | otherwise = n

isPalindromic :: Integer -> Bool
isPalindromic n  = (num) == (reverse num)
                        where num = show n
