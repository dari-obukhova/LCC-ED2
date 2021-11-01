(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Cadena = E | L Char | N Cadena Cadena deriving Show

palindromo :: Cadena -> Bool

palindromo (N (L x) (L y)) | x == y = True
                           | otherwise = False
palindromo E = True
palindromo (L x) = True
palindromo (N (N (L x) (L y)) (L z)) | x == z = True
                                     | otherwise = False
palindromo (N (L x) (N (L y) (L z))) | x == z = True
                                     | otherwise = False
palindromo (N c1 c2) = let c1' = esPrefijo (reverse' c2) c1
                       in case c1' of (False, _) -> False
                                      (True, t') -> palindromo t'

ejemplo = N (N (N (L 'a') (L 'b')) (N (L 'c') (L 'd'))) (N (N (L 'e') (L 'f')) (L 'g'))
ejemplo1 = N (N (N (L 'a') (L 'b')) (N (L 'c') (L 'd'))) (N (N (L 'c') (L 'b')) (L 'a'))
ejemplo2 = N (L 'a') (N (N (L 'c') (L 'b')) (N (L 'c') (L 'a')))

reverse' E = E
reverse' (L a) = (L a)
reverse' (N l r) = let (l',r') = reverse' l ||| reverse' r in (N r' l')

--esPrefijo :: Eq a => Tree a -> Tree a -> (Bool, Tree a)

esPrefijo _ E = (False, E)
esPrefijo E t = (False, t)
esPrefijo (L x) (L y) | x == y = (True, E)
                      | otherwise = (False, (L y))
esPrefijo (L x) t@(N E r) = let (bool, t') = esPrefijo (L x) r in case bool of True -> (True, t')
                                                                               False -> (False, t)
esPrefijo (L x) t@(N l r) = let (bool, t') = esPrefijo (L x) l in case bool of True -> case t' of E -> (True, r)
                                                                                                  _ -> (True, (N t' r))
                                                                               False -> (False, t)
esPrefijo (N l1 r1) t = let (bool, t') = esPrefijo l1 t in case bool of True -> esPrefijo r1 t'
                                                                        False -> (False, t)
