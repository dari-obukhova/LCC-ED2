(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

tam :: Tree a -> Int
tam E = 0
tam (N s _ _ _) = s

dropWhile' :: (a -> Bool) -> Tree a -> Tree a

dropWhile' p E = E
dropWhile' p (N s l x r) = let l' = dropWhile' p l
                               r' = dropWhile' p r
                           in if (p x) then if (tam l' == 0) then r' else (N (tam l' + tam r + 1) l' x r) else (N (tam l' + tam r + 1) l' x r)

ejemplo = (N 6 (N 3 (N 1 E 1 E) 3 (N 1 E 5 E)) 4 (N 2 (N 1 E 7 E) 8 E)) :: Tree Int
