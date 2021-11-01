(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | L a | N (Tree a) (Tree a) deriving Show

--a

esPrefijo :: Eq a => Tree a -> Tree a -> (Bool, Tree a)

esPrefijo _ E = (False, E)
esPrefijo E t = (False, t) --no se va a romper el caso recursivo??
esPrefijo (L x) (L y) | x == y = (True, E)
                      | otherwise = (False, (L y))
esPrefijo (L x) t@(N E r) = let (bool, t') = esPrefijo (L x) r in case bool of True -> (True, t')
                                                                               False -> (False, t)
esPrefijo (L x) t@(N l r) = let (bool, t') = esPrefijo (L x) l in case bool of True -> (True, (N t' r))
                                                                               False -> (False, t)
esPrefijo (N l1 r1) t = let (bool, t') = esPrefijo l1 t in case bool of True -> esPrefijo r1 t'
                                                                        False -> (False, t)

ejemplo = N (N (N (L 3) (L 4)) (L 2)) (N (L 5) (L 7))
ejemploPref1 = N (L 3) (L 4)
ejemploPref2 = N (N (L 3) (L 4)) (L 2)
ejemploSub1 = N (N (L 2) (L 5)) (L 7)
ejemploSub2 = N (N (L 3) (L 5)) (L 5) 

--b

tailT :: Tree a -> Tree a

tailT E = E
tailT (L x) = E
tailT (N (L x) (L y)) = (L y)
tailT (N l r) = case l of E -> tailT r
                          _ -> (N l' r) where l' = tailT l


esSubSeq :: Eq a => Tree a -> Tree a -> Bool

esSubSeq t E = False
esSubSeq (L x) (L y) | x == y = True
                     | otherwise = False
esSubSeq (N l r) (L y) = False
esSubSeq t1 t2 = let (bool, t) = esPrefijo t1 t2
                 in bool || (esSubSeq t1 (tailT t2))
