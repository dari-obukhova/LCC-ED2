{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo = (fromList [-2,-4,6,-1,5,-7,2]) :: A.Arr Int

--maxBalance :: (Seq s) => s Int -> Int

maxBalance s = (\(_,_,m,_) -> m)  (maxBalance' s)

maxBalance' s = case showtS s of
  EMPTY -> (0,0,0,0)
  ELT a -> let a' = max a 0 in (a', a', a', a)--(a,a,a,a) -- suf, pref, max, total
  NODE l r -> let (l', r') = maxBalance' l ||| maxBalance' r
                  combine (s1, p1, m1, t1) (s2, p2, m2, t2) = (max s2 (s1+t2), max p1 (t1+p2), max (max m1 m2) (s1+p2), t1+t2)
              in combine l' r'
