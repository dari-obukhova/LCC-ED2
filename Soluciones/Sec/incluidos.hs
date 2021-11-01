{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo = (fromList [(1,3), (1,2), (2,3), (2,2), (3,2)]) :: A.Arr (Int, Int)
{--
incluidos s = let s1 = tabulateS (\i -> f (nthS s i) (dropS s (i+1))) ((lengthS s)-1) :: A.Arr (A.Arr Integer)
                  f (x,y) xs = mapS (f2 (x,y)) xs
                  f2 (x1,y1) (x2,y2) = if (x2>=x1 && (x2+y2) <= (x1+y1)) then 1 else 0
              in s1
--}

incluidos s = let s1 = tabulateS (\i -> f (nthS s (l - i - 1)) (takeS s (l-1-i))) (l-1) :: A.Arr Int
                  l = lengthS s
                  f (x,y) xs = if (lengthS (filterS (incluido (x,y)) xs)) >= 1 then 1 else 0
                  incluido (x2,y2) (x1,y1)  = if (x2>=x1 && (x2+y2) <= (x1+y1)) then True else False
              in reduceS (+) 0 s1
