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

maxS :: forall s a . Seq s => (a -> a -> Ordering) -> s a -> Int

maxS f s = second (x,y) where
  s' = tabulateS f' (lengthS s) :: s (a, Int)
  f' i = (nthS s i, i)
  f2 (a,_) (b,_) = f a b
  s1 = sort f2 s'
  (x,y) = nthS s1 ((lengthS s) - 1)
  second (x,y) = y


ejemplo = (fromList [(5,0), (7,1), (12,3), (7,9), (22,10), (30,10), (10,20)]) :: A.Arr (Int, Int)

--infoCo :: Seq (Int, Int) -> (Int, Seq Int)

infoCo s = let (xs, x) = scanS f (0,0) s
               f (x1,y1) (x2,y2) = ((x1+x2), y1+y2)
               s2 = appendS (dropS xs 1) (singletonS x)
               s3 = mapS (\(x,y) -> x-y) s2
           in (((maxS compare s3) + 1), s3)
