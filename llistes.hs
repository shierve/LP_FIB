ones :: [Integer]
ones = iterate (+0) 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = 0:(foldr (\n acc -> n:(-n:acc)) [] (filter (>0) nats))

triangulars :: [Integer]
triangulars = scanl (\acc n -> acc+n) 0 (filter (>0) nats)

factorials :: [Integer]
factorials = scanl (\acc n -> acc*n) 1 (filter (>0) nats)

fibs :: [Integer]
fibs = foldr (\n acc -> n ++ acc) [] fibPairs
    where fibPairs = iterate (\[a,b] -> [a+b, a+b+b]) [0,1]

primes :: [Integer]
primes = filter (isPrime) (filter (>1) nats)
    where isPrime n = all (\v -> mod n v /= 0) (filter (>1) (takeWhile (<= (div n 2)) nats))

merge :: [Integer] -> [Integer] -> [Integer]
merge (x : xs) (y : ys)
    | x > y     = y : (merge (x:xs) ys)
    | x == y    = x : (merge xs ys)
    | otherwise = x : (merge xs (y:ys))
merge [] l = l
merge l [] = l

hammings :: [Integer]
hammings = 1:((map (*2) hammings)`merge`(map (*3) hammings)`merge`(map (*5) hammings))

lookNsay :: [Integer]
lookNsay = map (\s -> read s :: Integer) (iterate (say) "1")
    where   say (x:xs) = (show (length (takeWhile (== x) xs)+1))++(x:(say (dropWhile (==x) xs)))
            say [] = []

tartaglia :: [[Integer]]
tartaglia = iterate (nextTartaglia) [1]
    where nextTartaglia l = 1:t
            where (t, _) = (foldr (f) ([],0) (l))
                    where f v (acc, last) = ((last+v):acc, v)
