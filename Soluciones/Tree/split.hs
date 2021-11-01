(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size E = 0
size (L x) = 1
size (N n _ _) = n

split :: (a -> Bool) -> Tree a -> (Tree (Int, a), Tree (Int, a))

split p t = split' p t 0

split' :: (a -> Bool) -> Tree a -> Int -> (Tree (Int, a), Tree (Int, a))

split' p (L a) n = if p a then (L (n, a), E) else (E, L (n, a))
split' p (N s l r) n = let ((l',l''), (r', r'')) = split' p l n ||| split' p r (n + size l)
                       in case l' of E -> case l'' of E -> (r', r'')
                                                      _ -> case r'' of E -> (r', l'')
                                                                       _ -> (r', N (size l'' + size r'') l'' r'')
                                     _ -> case r' of E -> case l'' of E -> (l', r'')
                                                                      _ -> case r'' of E -> (l', l'')
                                                                                       _ -> (l', N (size l'' + size r'') l'' r'')
                                                     _ -> case l'' of E -> (N (size l' + size r') l' r', r'')
                                                                      _ -> case r'' of E -> (N (size l' + size r') l' r', l'')
                                                                                       _ -> (N (size l' + size r') l' r', N (size l'' + size r'') l'' r'')



ejemplo = N 8 (N 4 (N 2 (L 11) (L 5)) (N 2 (L 4) (L 7))) (N 4 (N 2 (L 17) (L 12)) (N 2 (L 15) (L 3)))
