fib :: Int -> Integer
fib n = infiFib !! n
    where infiFib = 0 : (1 : (zipWith (+) infiFib (tail infiFib)))
