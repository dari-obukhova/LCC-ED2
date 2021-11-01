(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data BTree a = E | L a |N Int (BTree a) (BTree a) deriving Show

ejemplo = N 4 (N 2 (L 1) (L 2)) (N 2 (L 3) (L 4)) :: BTree Int

mapIndex :: (a -> Int -> b) -> BTree a -> BTree b

mapIndex f t = mapIndex' 0 f t where
  mapIndex' n f E = E
  mapIndex' n f (L x) = L (f x n)
  mapIndex' n f (N s l r) = let (l', r') = mapIndex' n f l ||| mapIndex' (n + size l) f r in (N s l' r')
  size E = 0
  size (L _) = 1
  size (N s _ _) = s

fromSlow :: Int -> Int -> Int -> BTree Int
fromSlow _ 0 _ = E
fromSlow n 1 k = (L n)
fromSlow n m k = mapIndex f (tabulate n m) where
  f x n = x + (div n k)
  tabulate n 1 = (L n)
  tabulate n m = N m (tabulate n (div m 2)) (tabulate n (m - (div m 2)))
