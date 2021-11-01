{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

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


ejemplo = (fromList [('a',5), ('b',4), ('a',4), ('a',30), ('b',7), ('b',10)]) :: A.Arr (Char, Int)
ejemplo2 = (fromList [('a',5), ('a',6), ('b',1), ('a',3), ('a',4), ('b',2)]) :: A.Arr (Char, Int)

--aumentos :: Seq s => s (Art, Int) -> (Art, Int)

aumentos s = let cantAument = mapS ( \(ch,xs) -> (lengthS (filterS (>0) (tabulateS (\i -> (nthS xs (i+1)) - (nthS xs i)) ((lengthS xs)-1) :: A.Arr Int)), ch) )  (collect s)
             in (\(x,y) -> (y,x)) (maxE compare cantAument)


--Ceci

{-
aumentos2 s = let n = lengthS s
                  s1 = tabulateS (\i -> (nthS s i, dropS s (i+1))) (n-1) :: A.Arr ((Char,Int), A.Arr (Char,Int))
                  s2 = mapS (\((a,p),xs) -> (a, appendS (singletonS p) (mapS snd (filterS (\(a',p') -> a==a') xs) ) ) ) s1
                  s3 = mapS (\(a,precios) -> (a)
              in s2

-}
