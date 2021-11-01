module ArrSeq where

  import Seq
  import qualified Arr as A

  instance Seq A.Arr where

    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS s = A.length s

    nthS s i = s A.! i

    tabulateS = A.tabulate

    mapS f s = tabulateS (\i -> f (nthS s i)) (lengthS s)

    filterS f s = joinS (mapS f' s) where
      f' x = if f x then (singletonS x)
           else emptyS

    appendS s1 s2 = joinS (tabulateS (\i -> if i<l1 then f (nthS s1 i) else f (nthS s2 (i-l1))) (l1 + l2)) where
      l1 = lengthS s1
      l2 = lengthS s2
      f = singletonS

    takeS s n = A.subArray 0 n s

    dropS s n = A.subArray n (lengthS s - n) s

    showtS s | lengthS s == 0 = EMPTY
             | lengthS s == 1 = ELT (nthS s 0)
             | otherwise = NODE (takeS s n) (dropS s n) where
                n = div (lengthS s) 2

    showlS s | lengthS s == 0 = NIL
             | otherwise = CONS (nthS s 0) (dropS s 1)

    joinS = A.flatten

    reduceS f b s | lengthS s == 0 = b
                  | lengthS s == 1 = f b (nthS s 0)
                  | otherwise = let s' = contractS f s
                                in reduceS f b s'

    contractS f s = joinS (tabulateS f' n) where
      n = (lengthS s)
      contractAux i = if even i then singletonS (f (nthS s i) (nthS s (i+1))) else emptyS
      f' = (\i -> if even n then contractAux i
                  else if i == (n-1) then singletonS (nthS s i)
                       else contractAux i)

    scanS f b s | lengthS s == 0 = (emptyS, b)
                | lengthS s == 1 = (singletonS b, f b (nthS s 0))
                | otherwise = let s1 = contractS f s
                                  (s2,x) = scanS f b s1
                                  r = expandS f s s2
                              in (r,x)

    expandS f s1 s2 = tabulateS f' n where
      n = (lengthS s1)
      f' = (\i -> if even i then nthS s2 (div i 2)
                  else f (nthS s2 (div i 2)) (nthS s1 (i-1)))

    fromList xs = A.fromList xs
