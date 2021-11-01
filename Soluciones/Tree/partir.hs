(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data Tree a = E | L a | N (Tree a) (Tree a) deriving Show

partir :: Tree a -> Tree (Tree a, a, Tree a)

partir t = zipTree3 (prefijos t) t (sufijos t)

sufijos :: Tree a -> Tree (Tree a)
sufijos t = sufijos' t E

sufijos' :: Tree a -> Tree a -> Tree (Tree a)
sufijos' (L _) s = L s
sufijos' (N l r) E = let (l', r') = sufijos' l r ||| sufijos' r E
                     in N l' r'
sufijos' (N l r) t = let (l', r') = sufijos' l (N r t) ||| sufijos' r t
                     in N l' r'

prefijos :: Tree a -> Tree (Tree a)

prefijos t = prefijos' t E

prefijos' :: Tree a -> Tree a -> Tree (Tree a)
prefijos' (L _) t = L t
prefijos' (N l r) E = let (l', r') = prefijos' l E ||| prefijos' r l
                      in N l' r'
prefijos' (N l r) t = let (l', r') = prefijos' l t ||| prefijos' r (N t l)
                      in N l' r'

ejemplo = N (N (N (L 3) (L 4)) (L 2)) (N (L 5) (L 7)) :: Tree Int

zipTree3 :: Tree (Tree a) -> Tree a -> Tree (Tree a) -> Tree (Tree a, a, Tree a)

zipTree3 (L t1) (L x) (L t2) = L (t1, x, t2)
zipTree3 (N l1 r1) (N l2 r2) (N l3 r3) = let (l',r') = zipTree3 l1 l2 l3 ||| zipTree3 r1 r2 r3
                                         in N l' r'


partir2 :: Tree a -> Tree (Tree a, a, Tree a)

partir2 t = partir2' t E E

partir2' (L x) tp ts = L (tp, x, ts)
partir2' t@(N l r) E E = let (l', r') = partir2' l E r ||| partir2' r l E
                         in N l' r'
partir2' t@(N l r) tp E = let (l', r') = partir2' l tp r ||| partir2' r (N tp l) E
                          in N l' r'
partir2' t@(N l r) E ts = let (l', r') = partir2' l E (N r ts) ||| partir2' r l ts
                          in N l' r'
partir2' t@(N l r) tp ts = let (l', r') = partir2' l tp (N r ts) ||| partir2' r (N tp l) ts
                           in N l' r'
