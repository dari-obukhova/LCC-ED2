{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

ejemplo1 = (fromList [(21,5,2,78), (22,11,0,79), (22, 12,3,84), (23, 22,7,13), (24, 25,10,17)]) :: A.Arr (Int, Int, Int, Int)


fechaMaxDif :: Seq s => s (Int, Int, Int, Int) -> s (Int, Int)

fechaMaxDif s = let s' = mapS (\(f,t_min,t_max,_) -> (t_min - t_max, f)) s --- MAX - MIN ???
                    maxT = reduceS f (0, minBound) s'
                    f (x1,fecha1) (x2,fecha2) = if x1>x2 then (x1,fecha1) else (x2,fecha2)
                in (\(temp, fecha) -> fecha) maxT

lluviaAcum s n = let s' = mapS (\(_,_,_,x) -> x) s
                     (xs, x) = scanS (+) 0 s'
                     s2 = appendS (dropS xs 1) (singletonS x) --suma de acc
                     accFecha = tabulateS (\i -> ((f (nthS s i)),(nthS s2 i))) (lengthS s) :: A.Arr (Int, Int)
                     f (fecha, _, _, _) = fecha
                     filtrados = filterS (\(fecha, acc) -> acc >= n) accFecha
                 in (\(fecha, acc) -> fecha) (nthS filtrados 0)
