eql :: [Int] -> [Int] -> Bool
eql l1 l2 = (length l1 == length l2) && (foldl (comp) True (zip l1 l2))
    where comp b (n, m) = (b && (n == m))

prod :: [Int] -> Int
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens = foldl (prodAux) 1
    where prodAux acc v
            | mod v 2 == 0  = acc*v
            | otherwise = acc

powersOf2 :: [Int]
powersOf2 = scanl (bytwo) 1 [1..]
    where bytwo acc _ = acc * 2

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct f1 f2 = foldl (scalarAux) 0 (zip f1 f2)
    where scalarAux acc (v1, v2) = acc + (v1*v2)
