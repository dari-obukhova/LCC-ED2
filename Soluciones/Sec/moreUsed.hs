{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

merge f s1 s2 | lengthS s1 == 0 = s2
              | lengthS s2 == 0 = s1
              | f (nthS s1 0) (nthS s2 0) == LT = appendS (singletonS (nthS s1 0)) (merge f (dropS s1 1) s2)
              | f (nthS s1 0) (nthS s2 0) == EQ = appendS (singletonS (nthS s1 0)) (merge f (dropS s1 1) s2)
              | f (nthS s1 0) (nthS s2 0) == GT = appendS (singletonS (nthS s2 0)) (merge f s1 (dropS s2 1))

sort f s = case showtS s of
  EMPTY -> emptyS
  ELT a -> singletonS a
  NODE l r -> let (l', r') = sort f l ||| sort f r
              in merge f l' r'

group' :: Ord a => Seq s => forall b . s (a, b) -> s (a, s b)
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


--type Doc = (Int, Seq String)

--type Index = Seq (String, Seq (Int,Int))

ejemplo = (fromList [(1, text1), (2, text2), (3, text3)]) :: A.Arr (Int, A.Arr String)

text1 = (fromList ["the", "big", "dog"]) :: A.Arr String
text2 = (fromList ["a", "big", "dog", "ate", "a", "hat"]) :: A.Arr String
text3 = (fromList ["i", "read", "a", "big", "book"]) :: A.Arr String

--makeIndex :: Seq s => s (Int, s String) -> s (String, s (Int,Int))

makeIndex s = let s1 = mapS (\(n, xs) -> (tabulateS (\i -> ((nthS xs i), (n, i))) (lengthS xs)) :: A.Arr (String, (Int,Int)))  s
                  s2 = collect (joinS s1)
              in s2

--moreUsed :: s (String, s (Int,Int)) -> String

moreUsed s = let s' = mapS (\(string, xs) -> ((lengthS xs),string)) s
                 maxim = reduceS f (0,"palabra") s'
                 f (k, w) (n, z) = if k >= n then (k,w) else (n, z)
             in (\(_,palabra) -> palabra) maxim
