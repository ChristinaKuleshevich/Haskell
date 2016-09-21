stutter [] = []
stutter (x:xs) = [x] ++ [x] ++ stutter xs

compress [] = []
compress (x:xs) = if x == head xs then x : compress (tail xs) else x : compress  xs  

 

intersect [] [] = []
intersect [_] [] = []
intersect [] [_] = []                   
intersect (x:xs) (y:ys) = if x == y then x : xs else []



isPrefixOf [] [] = []
isPrefixOf (x:xs) (y:ys) = if x == y and head xs == head ys then True else False

