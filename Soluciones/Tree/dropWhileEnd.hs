(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

ejemplo = (N 6 (N 3 (N 1 E 1 E) 3 (N 1 E 5 E)) 4 (N 2 (N 1 E 7 E) 8 E)) :: Tree Int

ejemplo1 = (N 6 (N 3 (N 1 E 1 E) 2 (N 1 E 5 E)) 4 (N 2 (N 1 E 7 E) 9 E)) :: Tree Int

size :: Tree a -> Int
size E = 0
size (N s _ _ _) = s

dropWhileEnd :: (a -> Bool) -> Tree a -> Tree a

dropWhileEnd p E = E

dropWhileEnd p (N n l x r) = let (l',r') = dropWhileEnd p l ||| dropWhileEnd p r
                             in if (p x) then case r' of E -> l'
                                                         _ -> N (size l + size r' + 1) l x r'
                                         else N (size l + size r' + 1) l x r'


--                             in if (p x) then if (size r' == 0) then l' else N (size l + size r' + 1) l x r' else N (size l + size r' + 1) l x r'
