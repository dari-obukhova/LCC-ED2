{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo = (fromList [5,7,3,1,9,2,7,6]) :: A.Arr Int

bits = (fromList [1,1,1,1,0,0,1,0]) :: A.Arr Int
bits2 = (fromList [1,1,1,1,1,1]) :: A.Arr Int

cantCeros :: (Seq s) => s Int -> Int

cantCeros s = lengthS (filterS (== 0) s)

--posUnos :: (Seq s) => s Int -> s Int

{- sin usar scan
posUnos s = let zeros = cantCeros s
                pos = tabulateS (\i -> if ((nthS s i) == 1) then if (i == 0) then zeros else ((nthS pos (i-1)) + 1) else if (i == 0) then zeros else (nthS pos (i-1))) (lengthS s)
            in pos

-}

posUnos s = let zeros = cantCeros s
                (xs, x) = scanS (+) (zeros-1) s
                s2 = appendS (dropS xs 1) (singletonS x)
            in s2

posCeros s = let invrs = mapS (\x -> if x == 1 then 0 else 1) s
                 (xs,x) = scanS (+) (-1) invrs
                 s2 = appendS (dropS xs 1) (singletonS x)
             in mapS (\x -> if x == (-1) then 0 else x) s2


posiciones :: (Seq s) => s Int -> s Int

posiciones bs = let posunos = posUnos bs
                    posceros = posCeros bs
                    s2 = tabulateS (\i -> if (nthS bs i) == 1 then (nthS posunos i) else (nthS posceros i)) (lengthS bs)
                in s2
