{-# LANGUAGE ScopedTypeVariables #-}
import Seq
import ListSeq
import Par
import ArrSeq
import qualified Arr as A

all' :: Seq s => Int -> s (Int, Int)

all' 0 = emptyS

--all' n = tabulateS (\i -> (tabulateS (\k -> (i, k+1) n :: A.Arr (Int, Int)))) n :: A.Arr (Int, Int)

all' n = joinS (tabulateS (\i -> f i) n) where
  f k = tabulateS (\i -> (k, (i+1))) (n-1)


pointsD2 n = filterS (\(x,y) -> x<y) (joinS (tabulateS (\i -> f i) n)) where
 f k = tabulateS (\i -> (k, (i+1))) (n-1)
