(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size E = 0
size (L x) = 1
size (N n _ _) = n

filterInd :: (a -> Bool) -> Tree a -> Tree (Int, a)

filterInd p t = filterInd' 0 p t

filterInd' :: Int ->  (a -> Bool) -> Tree a -> Tree (Int, a)

filterInd' n p E = E
filterInd' n p (L a) = if p a then L (n, a)
                              else E
filterInd' n p (N s l r) = let (l', r') = filterInd' n p l ||| filterInd' (n + (size l)) p r
                           in case l' of E -> r'
                                         _ -> case r' of E -> l'
                                                         _ -> N ((size l') + (size r')) l' r'


ejemplo = N 8 (N 4 (N 2 (L 11) (L 5)) (N 2 (L 4) (L 7))) (N 4 (N 2 (L 17) (L 12)) (N 2 (L 15) (L 3)))
