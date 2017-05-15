myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ ini [] = ini
myFoldl f ini (x:xs) = myFoldl f (f ini x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ini [] = ini
myFoldr f ini (x:xs) = f x (myFoldr f ini xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil cond f x
    | (cond x)  = x
    | otherwise = myUntil cond f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f l = foldr (\y ys -> (f y):ys) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = foldr (faux) [] l
    where faux v acc
            | f v       = v:acc
            | otherwise = acc

myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = foldr (func) True l
    where func v acc = (f v) && acc

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = foldr (func) False l
    where func v acc = (f v) || acc

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map (applyF) (zip xs ys)
    where applyF (a,b) = f a b
