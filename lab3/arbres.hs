data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ t1 t2) = 1+(size t1)+(size t2)

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a a1 a2) (Node b b1 b2)  = (a == b) && (equal a1 b1) && (equal a2 b2)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a a1 a2) (Node b b1 b2)  = (a == b) && (((isomorphic a1 b1) && (isomorphic a2 b2)) || ((isomorphic a1 b2) && (isomorphic a2 b1)))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a a1 a2) = a:((preOrder a1)++(preOrder a2))

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a a1 a2) = ((postOrder a1)++(postOrder a2))++[a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a a1 a2) = ((inOrder a1)++[a]++(inOrder a2))

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst t = breadthAux [t]
    where
        breadthAux [] = []
        breadthAux ((Node a a1 a2):xs) = a:(breadthAux (xs++[a1]++[a2]))
        breadthAux ((Empty):xs) = breadthAux xs

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:xs) (ys) = Node x (build (take (length left) xs) (left)) (build (drop (length left) xs) (tail $ dropWhile (/= x) ys))
    where
        left = takeWhile (/= x) ys

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ t1 Empty = t1
overlap _ Empty t2 = t2
overlap f (Node a a1 a2) (Node b b1 b2) = Node (f a b) (overlap f a1 b1) (overlap f a2 b2)
