(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | L a | N (Tree a) (Tree a) deriving Show

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr
 where mr E = e
       mr (L a) = f a
       mr (N l r) = let (l', r') = mr l ||| mr r
                    in g l' r'

sufijos :: Tree a -> Tree (Tree a)
sufijos t = sufijos' t E

sufijos' :: Tree a -> Tree a -> Tree (Tree a)
sufijos' (L x) t = L t
sufijos' t@(N l r) E = (N l' r') where (l', r') =  sufijos' l r ||| sufijos' r E
sufijos' t1@(N l1 r1) t2@(N l2 r2) = (N l' r') where (l', r') = sufijos' l1 (N r1 t2) ||| sufijos' r1 t2

--ejemplo = N (N (L 1) (L 2)) (N (L 3) (L 4)) :: Tree Int

prefijos :: Tree a -> Tree (Tree a)
prefijos t = prefijos' E t

prefijos' :: Tree a -> Tree a -> Tree (Tree a)
prefijos' t (L x) = L t
prefijos' E (N l r) = (N l' r') where (l', r') = prefijos' E l ||| prefijos' l r
prefijos' t (N l r) = (N l' r') where (l', r') = prefijos' t l ||| prefijos' (N t l) r

separar :: Tree Int -> Tree (Tree Int, Int, Tree Int)
separar t = zipTree (prefijos t) t (sufijos t) where
  zipTree (N l1 r1) (N l2 r2) (N l3 r3) = N l r where (l,r) = zipTree l1 l2 l3 ||| zipTree r1 r2 r3
  zipTree (L x) (L y) (L z) = L (x, y, z)

addInfo :: Tree Int -> Tree (Bool, Int, Bool)
addInfo E = E
addInfo t = addInfo' (separar t) where
  addInfo' :: Tree (Tree Int, Int, Tree Int) -> Tree (Bool, Int, Bool)
  addInfo' (N l r) = (N l' r') where (l', r') = addInfo' l ||| addInfo' r
  addInfo' (L (pref, x, suf)) = (L (b1, x, b2)) where (b1, b2) = (mapreduce (<x) (||) (False) (pref)) ||| (mapreduce (>x) (||) (False) (suf))
