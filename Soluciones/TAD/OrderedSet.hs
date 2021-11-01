(|||) :: a -> b -> (a,b)
x ||| y = (x,y)


data BST a = E | N (BST a) a (BST a) deriving Show

ejemplo = N l1 10 r1

l1 = N l2 6 r2

l2 = N (N E 3 E) 4 (N E 5 E)

r2 = N (N E 7 E) 8 (N E 9 E)

r1 = N (N (N E 11 E) 12 (N E 13 E)) 15 (N (N E 17 E) 18 (N E 21 E))


split :: Ord a => a -> BST a -> (BST a, Maybe a, BST a)

split n E = (E, Nothing, E)

split n (N l x r) = if n == x then (l, Just x, r)
                              else if n > x then let (l', x', r') = split n r in ((N l x l'), x', r')
                                            else let (l', x', r') = split n l in (l', x', (N r' x r))

join :: Ord a => BST a -> BST a -> BST a

{-
join t E = t
join E t = t
join t1@(N l x r) t2@(N l' y r') = if x < y then let (l2, z, r2) = split x l' in (N (join l l2) x (join r (N r2 y r')))
                                            else let (l2, z, r2) = split x r' in (N (join l (N l' y l2)) x (join r r2))

-}
join t E = t
join E t = t
join (N l x r) t = let (l', y, r') = split x t in N (join l l') x (join r r')

delete :: Ord a => a -> BST a -> BST a

delete x E = E

delete x t = let (l, x', r) = split x t in case x' of Nothing -> t
                                                      _ -> join l r
