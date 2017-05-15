quadrats::[Integer]
quadrats = map (\n -> n*n) [1..]

sumQuadrats::Integer -> Bool
sumQuadrats n = auxSum n (takeWhile (<n) quadrats)
    where
        auxSum x [] = False
        auxSum x l = findSum x l || auxSum x (tail l)
        findSum _ [] = False
        findSum sum (x:xs)
            | x == sum = True
            | x > sum = False
            | otherwise = findSum (sum-x) xs

conway::[Integer]
conway = 1:(1:(map (\n -> (conway !! (fromIntegral ((last n)-1))) + (conway !! ( fromIntegral (n-(last n)-1))) ) [3..]))
    where last n = (conway !! ( fromIntegral (n-2)))

dc::(a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dc trivial resol parteix combina problema
    | trivial problema = resol problema
    | otherwise = combina problema (map (\subproblema -> dc trivial resol parteix combina subproblema) (parteix problema))

quicksort::Ord a => [a] -> [a]
quicksort = dc trivial resol parteix combina
    where
        trivial l = ((length l) < 2)
        resol p = p
        parteix (x:xs) = [ filter (<x) xs, filter (>=x) xs ]
        combina (x:_) [l1, l2] = l1 ++ (x:l2)

data GTree a = Node a [GTree a] deriving (Show)

flat::Eq a => GTree a -> GTree a
flat (Node n []) = Node n []
flat (Node n l) = substitute (Node n (map (flat) l))
    where
        getVal (Node v _) = v
        getChildren (Node _ c) = c
        substituteList _ [] = []
        substituteList m (x:xs)
            | m == (getVal x) = ((getChildren x)++(substituteList m xs))
            | otherwise = (x:(substituteList m xs))
        substitute (Node m c) = (Node m (substituteList m c))

eqTree :: Eq a => GTree a -> GTree a -> Bool
eqTree (Node a al) (Node b bl) = (a==b) && ((length al) == (length bl)) && (equalList al bl)
    where
        equalList [] _ = True
        equalList (x:xs) l2 = (x `inList` l2) && (equalList xs l2)
        inList _ [] = False
        inList a (x:xs) = ((a == x) || (inList a xs))

instance Eq a => Eq (GTree a)
    where
        (==) a b = eqTree (flat a) (flat b)
