module ListSeq where

import Seq
import Par

instance Seq [] where

  emptyS = []

  singletonS x = [x]

  lengthS xs = (reduceS (+) 0 . map (\_ -> 1)) xs

  nthS (x:_) 0 = x
  nthS (_:xs) n = nthS xs (n-1)

  tabulateS f n = tab f 0 where
    tab f k | k == n = []
            | otherwise = let (y, ys) = f k ||| tab f (k+1)
                          in y:ys

  mapS f [] = []
  mapS f (x:xs) = let (y, ys) = f x ||| mapS f xs
                   in y:ys

  filterS f [] = []
  filterS f (x:xs) = let (y, ys) = f x ||| filterS f xs
                      in if y then x:ys
                              else ys

  appendS [] ys = ys
  appendS (x:xs) ys = x : appendS xs ys

  takeS [] _ = []
  takeS xs 0 = []
  takeS (x:xs) n = x: takeS xs (n-1)

  dropS xs n | n <= 0 = xs
  dropS [] _ = []
  dropS (_:xs) n = dropS xs (n-1)

  showtS [] = EMPTY
  showtS [x] = ELT x
  showtS xs = NODE l1 l2 where
    n = div (lengthS xs) 2
    (l1, l2) = takeS xs n ||| dropS xs n

  showlS [] = NIL
  showlS (x:xs) = CONS x xs

  joinS = reduceS (appendS) []

  reduceS f b [] = b
  reduceS f b [x] = f b x
  reduceS f b xs = reduceS f b (contractS f xs)

  contractS f [] = []
  contractS f [x] = [x]
  contractS f (x:y:xs) = let (z, zs) = (f x y) ||| (contractS f xs)
                         in z:zs

  scanS f b [] = ([],b)
  scanS f b [x] = ([b], f b x)
  scanS f b xs = let (ys, y) = scanS f b (contractS f xs)
                 in (expandS f xs ys, y)

  expandS f [] _ = []
  expandS f _ [] = []
  expandS f [_] [x] = [x]
  expandS f (x:_:xs) (y:ys) = y: (f y x) : expandS f xs ys

  fromList = id
