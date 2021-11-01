(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

-- mi exam - ej 2

data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

tabulate :: (Int -> a) -> Int -> Tree a
tabulate f 0 = E
tabulate f 1 = N 1 E (f 0) E
tabulate f n = let m = div n 2
                   (l,r) = tabulate f m ||| tabulate (f.(+(m+1))) (n-m-1)
                   in N n l (f m) r

tam  :: Tree a -> Int
tam E = 0
tam (N n l x r) = n

appendT :: Tree a -> Tree a -> Tree a
appendT E t = t
appendT t E = t
appendT t1@(N n1 l1 x r1) t2@(N n2 l2 y r2) = N (n1+n2) (appendT l1 r1) x t2

t1 = (N 6 (N 3 (N 1 E 1 E) 2 (N 1 E 3 E) ) 4 (N 2 (N 1 E 6 E) 5 E))
t2 = (N 2 (N 1 E 8 E) 7 E)


--N 8 (N 5 (N 2 E 1 (N 1 E 3 E)) 2 (N 2 (N 1 E 6 E) 5 E)) 4 (N 2 (N 1 E 8 E) 7 E)

--N 8 (N 1 E 8 E) 7 (N 6 (N 3 (N 1 E 1 E) 2 (N 1 E 3 E)) 4 (N 2 (N 1 E 6 E) 5 E))

create :: (Int -> Bool) -> (Int -> a) -> Int -> Tree a
create p f 0 = E
create p f 1 = if (p 0) then N 1 E (f 0) E else E
create p f n = let m = div n 2
                   (l,r) = create p f m ||| create (p.(+(m+1))) (f.(+(m+1))) (n-m-1)
               in if (p m) then N (tam l + tam r + 1) l (f m) r
                           else (appendT l r)
