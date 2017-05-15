
diffSqrs :: Integer -> Integer
diffSqrs i = (sumInt*sumInt) - (sumqQuad)
    where
        sumInt = div (i*(i+1)) 2
        sumqQuad = foldl (\acc v -> acc + (v*v)) 0 [1..i]

pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets s = [ [a, b, (a*a)+(b*b)] | (a,b) ]
