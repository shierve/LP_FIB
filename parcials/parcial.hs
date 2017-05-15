allsets :: a -> [[a]]
allsets n = iterate (n:) [n]

alldivisors:: Int -> [[Int]]
alldivisors n =
