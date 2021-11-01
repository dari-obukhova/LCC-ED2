(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | N (Tree a) a (Tree a) deriving Show


delNeg :: Tree Int -> Tree Int

delNeg E = E
delNeg t@(N l x r) = delNeg' 0 t

delNeg' :: Int -> Tree Int -> Tree Int

delNeg' n E = E
delNeg' n (N l x r) | x > 0 = let (l', r') = delNeg' n l ||| delNeg' x r in (N l' x r')
                    | otherwise = let raiz (N _ r _) = r
                                      raiz E = n
                                      l' = delNeg' n l
                                      x' = raiz l'
                                      r' = delNeg' x' r
                                  in (N l' x' r')

ejemplo = N (N (N E (-1) E) 2 (N E (-2) E)) 3 (N (N E 4 E) (-5) (N E 1 E)) :: Tree Int
