data Queue a = Queue [a] [a]
    deriving (Show)

instance Eq a => Eq (Queue a)
    where
        (==) = equalQueue

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push l (Queue (xs) (ys)) = Queue xs (l:ys)

pop :: Queue a -> Queue a
pop (Queue (x:xs) (ys)) = Queue xs ys
pop (Queue [] (ys)) = Queue (tail $ reverse ys) []

top :: Queue a -> a
top (Queue (x:xs) _ ) = x
top (Queue [] (ys)) = last ys

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False

equalQueue :: Eq a => Queue a -> Queue a -> Bool
equalQueue (Queue [] []) q2 = empty q2
equalQueue _ (Queue [] []) = False
equalQueue q1 q2 = ((top q1 == top q2) && equalQueue (pop q1) (pop q2))
