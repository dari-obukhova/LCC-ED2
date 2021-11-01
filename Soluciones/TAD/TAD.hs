tad Array 

set n a xs = if (dimension xs >=n) then xs else set n a xs

get n (set m x xs) = if n == m then x else get n xs

get n (new m x) = if n < m then Just x else Nothing 

slicing i j (new n x) = if j > i then new (j -i) x else new 0 x 

slicing i j (set n v a) = if i < dimension and j < dimension then get' i j (set n v a) (new (j-i) v)  


tad Graph 

neighbours (null) = vacio 

neighbours (addVertex g x) y = neighbours g y 

neighbours (addEdge g (v,w)) y = if y == v then insertar w (neighbours g y) 
                                           else if y == w then insertar v (neighbours g y) 


isPath null x y = False 

isPath (addVertex g v) x y = isPath g x y

isPath (addEdge g (v,w)) x y = if v == x and w == y or v == y and w == x then True 
                               else if v == x then isPath g w y
                                              else if w == x then isPath g v y 
                                                             else if v == y then isPath g w x
                                                                            else if w == y then isPath g v x
                                                                                           else isPath g x y 

tad MultiDic 
                                                                                           
values k (create null) = vacio 

values k (create (cons (k',v) xs)) = if k == k' then insertar v (values k (create xs))
                                                else values k (create xs) 
                                                
delete (k,v) (create null) = (create null)

delete (k,v) (create (cons (k', v') xs) ) = if k == k' and v == v' then (create xs) 
                                                                   else deleteFromList 
                                                                   
delete


tad Ordered Set 

split x empty = empty, Nothing, empty 

split x xs = if find x xs then (menores x xs), Just x, (mayores x xs)
             else (menores x xs), Nothing, (mayores x xs) 
             
             
menores x empty = empty 

menores x (insert y ys) = if x > y then insert y (menores x ys) 
                                   else empty 
                                   
mayores x empty = empty 

mayores x (insert y ys) = if x < y then insert y (mayores x ys) 
                                   else empty 

  



