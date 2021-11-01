{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo = (fromList [1000, 500, 2100, 1600, 300, 1000]) :: A.Arr Int

--entradas :: Seq s => s Int -> Float -> (Float, Int)

entradas s = let (xs, x) = scanS (+) 0 s
                 s1 = appendS (dropS xs 1) (singletonS x)
                 s2 = tabulateS (\i -> div (nthS s1 i) (i+1)) (lengthS s1) :: A.Arr Int
             in s2
