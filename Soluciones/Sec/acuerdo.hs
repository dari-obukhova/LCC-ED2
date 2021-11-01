{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo = (fromList [10,14,21,25,35,40,50,55]) :: A.Arr Int

(//) :: Int -> Int -> Float
x // y = fromIntegral x / fromIntegral y

acuerdo :: Seq s => s Int -> Int -> Int -> (Int, Float)

acuerdo s maximo fijo = let (xs, x) = scanS (+) 0 s
                            s2 = appendS (dropS xs 1) (singletonS x)
                            s3 = filterS (<maximo) s2
                            cantfijos = (lengthS s) - (lengthS s3)
                            acumuladoConIntereses = nthS s3 ((lengthS s3) - 1)
                        in (acumuladoConIntereses + (cantfijos * fijo), (cantfijos // (lengthS s)) * 100)
