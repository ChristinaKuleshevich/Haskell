stutter [] = []
stutter (x:xs) = [x] ++ [x] ++ stutter xs

compress [] = []
compress (x:xs) = if x == head xs then x : compress (tail xs) else x : compress  xs  

-- | compress (stutter "Hello") == (compress . stutter) "Hello" 


findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f xs = myFindIndices 0 f xs where
  myFindIndices _ _ []     = []
  myFindIndices n f (x:xs) =
    (if f x then [n] else []) ++ myFindIndices (n+1) f xs
    -- if f x then n : myFindIndices (n+1) f xs else myFindIndices (n+1) f xs

findIndices' f xs = map fst (filter (f . snd) (zip [0..] xs))

findIndices'' f xs = [ i | (i, x) <- zip [0..] xs, f x ]

 -- findIndices (== 0) [1,2,0,3,0] -> [2,4]

intersect []     _  = []
intersect (x:xs) ys = if elem x xs
                      then intersect xs ys
                      else if elem x ys
                           then x : intersect xs ys
                           else intersect xs ys

intersect' []     _  = []
intersect' (x:xs) ys = if not (elem x xs) && elem x ys
                       then x : intersect xs ys
                       else intersect xs ys


isPrefixOf [] _  = True
isPrefixOf _  [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
  --if x == y && isPrefixOf xs ys then True else False

isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)


isSuffixOf' _ [] = False
isSuffixOf' xs ys = (isPrefixOf xs ys || isSuffixOf xs (tail ys))


-- | [x1, x2, x3] dot [y1, y2, y3] = x1 * y1 + x2 * y2 + x3 * y3
dot xs ys = sum (map (\ (x, y) -> x * y) (zip xs ys))


dot' xs ys = sum (zipWith (*) xs ys)


dot'' xs ys = sum $ map foo (zip xs ys) -- sum (map foo (zip xs ys))
  where foo (x, y) = x * y        


increasing [] = True
increasing [x] = True
increasing (x:xs) = if x < head xs then increasing xs else False
 -- x < head xs && increasing xs 

decimate xs = map snd $ filter ((/=0) . (`mod` 10) . fst) $ zip [1..] xs

decimate' :: [a] -> [a]
decimate' xs = select (/= 0) (map (`mod` 10) [1..length xs]) xs

-- | Функция select берет функцию как первый аргумент, применяет ее к каждому
-- элементу первого списка, и если результат правда, возвращает элемент с той же
-- позицией из второго списка, иначе этот элемент со второго списка игнорируется
select :: (a -> Bool) -> [a] -> [b] -> [b]
select f xs ys = map snd $ filter (f . fst) $ zip xs ys


-- encipher [’A’..’Z’] ['a'..'z'] "THIS"
encipher xs ys zs = map encipherElem zs where
  encipherElem z = head $ select (z==) xs ys
    -- map fst $ filter ((z==) . snd) $ zip ys xs

prefixSum zs = prefixSumAcc 0 zs where
  prefixSumAcc _ [] = []
  prefixSumAcc acc (x:xs) = (acc + x):prefixSumAcc (acc + x) xs

-- prefixSum' xs = reverse $ snd $ foldl (\(acc, xs) y -> (acc + y, (acc + y):xs)) (0, []) xs
-- prefixSum'' xs = tail $ reverse $ foldl (\xs y -> (head xs + y):xs) [0] xs

-- numbers [1,2,3,4] -> 1234
numbers xs = numbersAcc 0 xs where
  numbersAcc acc [] = acc
  numbersAcc acc (y:ys) = numbersAcc (acc*10 + y) ys

numbers' :: (Num b, Foldable t) => t b -> b
numbers' = foldl (\ acc y -> acc * 10 + y) 0
