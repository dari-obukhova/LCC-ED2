{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A


--1
--a

(//) :: Int -> Int -> Float
x // y = fromIntegral x / fromIntegral y

s1a = (fromList [2,4,3,7,9]) :: A.Arr Int

promedios :: (Seq s) => s Int -> s Float

promedios s = let (xs, n) = scanS (+) 0 s
                  xs' = appendS (dropS xs 1) (singletonS n)
              in tabulateS (\i -> (nthS xs' i) // (i + 1)) (lengthS xs')

--b

s1b = (fromList [1, 2, 5, 3, 5, 2, 7, 9]) :: A.Arr Int

mayores :: (Seq s) => s Int -> Int

mayores s = let (xs, n) = scanS max 0 s
                xs' = appendS (dropS xs 1) (singletonS n)
                k = (reduceS (+) 0 (tabulateS (\i -> if (nthS s i) == (nthS xs' i) then 1 else 0) (lengthS xs') :: A.Arr Int ))
            in (k - 1)

--2

fibSeq :: (Seq s) => Int -> s Int

fibSeq n = let mm (a1,b1,c1,d1) (a2,b2,c2,d2) = (a1*a2 + b1*c2, a1*b2+ b1*d2, c1*a2+d1*c2, c1*b2+d1*d2)
               s = tabulateS (\i -> (1,1,1,0)) n
               (ss, x) = scanS mm (1,0,0,1) s
               fib = appendS (dropS ss 1) (singletonS x)
           in mapS (\(_,_,_,x) -> x) fib

--3

s3 = (fromList [2,3,4,7,5,2,3,2,6,4,3,5,2,1]):: A.Arr Int

--aguaHist :: (Seq s) => s Int -> Int

aguaHist s = let (maxL, x) = scanS max 0 s
                 l = lengthS s
                 rs = tabulateS (\i -> nthS s (l - 1 - i)) l :: A.Arr Int
                 (maxR, y) = scanS max 0 rs
                 ms = tabulateS (\i -> max 0 ((min (nthS maxL i) (nthS maxR (l-1-i))) - (nthS s i))) l :: A.Arr Int
             in reduceS (+) 0 ms

--4

data Paren = Open | Close

s4 = (fromList [Open, Close, Open, Close]) :: A.Arr Paren
s4' = (fromList [Close, Close, Open, Open]) :: A.Arr Paren
s4'' = (fromList [Close, Open, Open, Close, Open, Close, Open, Close, Close, Open, Close]) :: A.Arr Paren

--a

matchParen :: (Seq s) => s Paren -> Bool

matchParen s = matchP s == (0,0)

matchP s = case showtS s of
  EMPTY -> (0,0)
  ELT Open -> (0,1)
  ELT Close -> (1,0)
  NODE l r -> let (l',r') = matchP l ||| matchP r in combine l' r' where combine (i,j) (i',j') = ((max 0 (i'-j)) + i, ((max 0 (j-i')) + j'))

--b

matchParen2 :: (Seq s) => s Paren -> Bool

matchParen2 s = let s' = mapS f s
                    f Open = 1
                    f Close = (-1)
                    (s'', total) = scanS (+) 0 s'
                in (lengthS (filterS (< 0) s'') == 0) && (total == 0)

--5

s5 = (fromList [9,3,5,1,3,4,5,6,8,1]):: A.Arr Int
s5' = (fromList [5,6,2,3,5,1,9]):: A.Arr Int
s5'' = (fromList [1,4,6,7,8,11,12,3]):: A.Arr Int

sccml :: (Seq s) => s Int -> Int

sccml s = ((\(_, _, m, _, _, _) -> m) (sccml' s)) - 1

sccml' s = case showtS s of
  EMPTY -> (0,0,0,0,0,0) -- (first element de sec, long de sec contgua al ppio, max long de sec contigua, total long de sec, last element, long de sec contigua al final)
  ELT x -> (x,1,1,1,x,1)
  NODE l r -> let (l',r') = sccml' l ||| sccml' r
                  combine (f1, lf1, max1, t1, l1, ll1) (f2, lf2, max2, t2, l2, ll2) = if l1 < f2 then (f1, (if lf1 == t1 then (lf1+lf2) else lf1), max (ll1 + lf2) (max max1 max2), (t1+t2), l2, (if ll2 == t2 then (ll2+ll1) else ll2)) else (f1, lf1, max max1 max2, (t1+t2), l2, ll2)
              in combine l' r'


sc = (fromList [1, (-5), 3, 7, (-2), (-10), 8]):: A.Arr Int

--sccml2 :: (Seq s) => s Int -> Int

sccml2 s = let (xs,x) = scanS (+) 0 s
               x' = appendS (dropS xs 1) (singletonS x)
               (ms,m) = scanS min maxBound x'
               m' = appendS (dropS ms 1) (singletonS m)
           in maxS compare (tabulateS (\i -> (nthS x' i) - (nthS m' i)) (lengthS s) :: A.Arr Int)

--6

s6 = (fromList [12, 4, 6, 3, 2]) :: A.Arr Int
s6' = (fromList [4, 6, 2]) :: A.Arr Int
s6'' = (fromList [1,2,3,4,5]) :: A.Arr Int

cantMultiplos :: (Seq s) => s Int -> Int

cantMultiplos s = lengthS (filterS (==0) (joinS (tabulateS (\i -> mapS (f (nthS s i)) (dropS s (i+1))) l)))  where
  l = lengthS s
  f n x = mod n x


--7

merge :: Seq s => (a -> a -> Ordering) -> s a -> s a -> s a

merge f s1 s2 | lengthS s1 == 0 = s2
              | lengthS s2 == 0 = s1
              | f (nthS s1 0) (nthS s2 0) == LT = appendS (singletonS (nthS s1 0)) (merge f (dropS s1 1) s2)
              | f (nthS s1 0) (nthS s2 0) == EQ = appendS (singletonS (nthS s1 0)) (merge f (dropS s1 1) s2)
              | f (nthS s1 0) (nthS s2 0) == GT = appendS (singletonS (nthS s2 0)) (merge f s1 (dropS s2 1))

sort ::  Seq s => (a -> a -> Ordering) -> s a -> s a

sort f s = case showtS s of
  EMPTY -> emptyS
  ELT a -> singletonS a
  NODE l r -> let (l', r') = sort f l ||| sort f r
              in merge f l' r'

maxE :: Seq s => (a -> a -> Ordering) -> s a -> a

maxE f s = nthS s' (lengthS s - 1) where
  s' = sort f s


maxS :: forall s a . Seq s => (a -> a -> Ordering) -> s a -> Int

maxS f s = second (x,y) where
  s' = tabulateS f' (lengthS s) :: s (a, Int)
  f' i = (nthS s i, i)
  f2 (a,_) (b,_) = f a b
  s1 = sort f2 s'
  (x,y) = nthS s1 ((lengthS s) - 1)
  second (x,y) = y

s7 = (fromList [(2,  "A"), (1, "B"), (1, "C"), (2, "D")]) :: A.Arr (Int, [Char])

group' :: Ord a => Seq s => s (a, b) -> s (a, s b)
group' s =
  let ss = mapS (\(i, j) -> singletonS (i, singletonS j)) s
  in reduceS comb emptyS ss
  where
    comb l r
      | lengthS l == 0 = r
      | lengthS r == 0 = l
      | otherwise = if (cmp lastL firstR) == EQ then (takeS l (lengthS l - 1)) `appendS` singletonS (fst lastL, (appendS (snd lastL) (snd firstR))) `appendS` dropS r 1
                    else appendS l r
                    where lastL = nthS l ((lengthS l) - 1)
                          firstR = nthS r 0
                          cmp (a, _) (c, _) = compare a c

collect :: (Seq s, Ord a) => s (a, b) -> s (a, s b)
collect s = let s' = sort cmp s
            in group' s'
            where cmp (a, _) (c, _) = compare a c


mapCollectReduce f1 f2 s = let pairs = joinS (mapS f1 s)
                               groups = collect pairs
                           in mapS f2 groups

text = (fromList ["Daria", "linda", "reintelligente", "linda", "hermosa", "reitelligente"]) :: A.Arr String

--8

notas = (fromList [("Daria", notas1), ("Nicole", notas2) , ("Rick", notas3), ("Ru", notas4), ("Lu", notas5), ("Alenka", notas6), ("Gaga", notas7)]) :: A.Arr (String, A.Arr Int)

notas1 = (fromList [100,100,90,80,90]) :: A.Arr Int
notas2 = (fromList [80,80,75,35,70]) :: A.Arr Int
notas3 = (fromList [21,74,33,45,55]) :: A.Arr Int
notas4 = (fromList [21,74,90,45,100]) :: A.Arr Int
notas5 = (fromList [21,65,20,45,55]) :: A.Arr Int
notas6 = (fromList [70,65,95,45,80]) :: A.Arr Int
notas7 = (fromList [21,70,95,80,55]) :: A.Arr Int

datosIngreso :: (Seq s) => s (String, s Int) -> s (Int, Int)

datosIngreso s = let promedios = mapS (\(name, xs) -> (grade (prom xs), prom xs)) s -- ??? referirse a prom xs y no calcularlo de nuevo
                     prom s = div (reduceS (+) 0 s) (lengthS s)
                     grade n = if n >= 70 then 1 else if n >= 50 then 2 else 3
                 in mapS (\(g, notas) -> (reduceS (+) 0 (mapS (\i -> 1) notas), maxE compare notas)) (collect promedios)

--9

text9 = (fromList [sec1, sec2, sec3]) :: A.Arr (A.Arr Char)

sec1 = (fromList ['o', 's', 'e', 'n']) :: A.Arr Char
sec2 = (fromList ['l', 'a', 'r', 'e', 'k']) :: A.Arr Char
sec3 = (fromList ['p', 'a', 'r', 'k']) :: A.Arr Char

text9' = (fromList [sec4, sec5]) :: A.Arr (A.Arr Char)

sec4 = (fromList ['d', 'a', 'd', 'a', 'd']) :: A.Arr Char
sec5 = (fromList ['l', 'a', 'd', 'a']) :: A.Arr Char

countCaract :: (Seq s) => s (s Char) -> s (Char, Int)

countCaract s = let pairs = mapS (\w -> (w,1)) (joinS s)
                    count = collect pairs
                in mapS (\(ch, cantidad) -> (ch, reduceS (+) 0 cantidad)) count

huffman :: (Seq s) => s (s Char) -> s (Int, s Char)

huffman s = collect (mapS (\(ch,n) -> (n,ch)) (countCaract s))
