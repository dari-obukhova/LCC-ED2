(|||) :: a -> b -> (a,b)
x ||| y = (x,y)

data BTree a = E | L a |N Int (BTree a) (BTree a) deriving Show

size :: BTree a -> Int
size E = 0
size (L x) = 1
size (N n _ _) = n

stripSufix :: Eq a => BTree a -> BTree a -> Maybe (BTree a)
stripSufix (L x) (L y) = if x == y then Just E
                                   else Nothing
stripSufix (L x) (N n l r) = if (size r > 0) then let p = stripSufix (L x) r in case p of Just t' -> Just (N (n-1) l t')
                                                                                          Nothing -> Nothing
                                             else stripSufix (L x) l
stripSufix (N n l r) t = let p = stripSufix r t in case p of Just t' -> stripSufix l t'
                                                             Nothing -> Nothing

ejemplo = N 4 (N 2 (L 1) (L 2)) (N 2 (L 3) (L 4)) :: BTree Int
ejemploSuf = N 2 (L 3) (L 4) :: BTree Int
