data AGTree a = Node a [AGTree a] deriving Show

maxAGT :: AGTree Int -> Int

maxAGT (Node x []) = x
maxAGT (Node x xs) = max x (maximum (map maxAGT xs))

ejemplo = Node 1 [Node 2 [Node 3 [Node 4 [], Node 5 [], Node 9 []], Node 15 [Node 21 [], Node 16 []], Node 35[]], Node 7 [Node 8[]], Node 21 []] :: AGTree Int
