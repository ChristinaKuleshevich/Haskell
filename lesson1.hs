
stutter [] = []
stutter (x:xs) = [x] ++ [x] ++ stutter xs

stutter' [] = []
stutter' (x:xs) = x:x:stutter' xs

compress [] = []
compress (x:xs) = if x == head xs then x : compress (tail xs)  else x : compress xs

findIndices f xs = myfindIndices 0 f xs where
  myfindIndices _ _ [] = []
  myfindIndices n f (x:xs) = (if f x then [n] else []) ++  myfindIndices (n+1) f xs 

intersect []      _ = []
intersect (x:xs) ys = if elem x xs then intersect xs ys
                                   else if elem x ys
                                        then x : intersect xs ys
                                        else intersect xs ys 

isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf (x:xs) (y:ys) = isPrefixOf (x:xs) (reverse (y:ys))


dot xs ys = sum (zipWith (*) xs ys)

increasing [] = True
increasing [x] = True
increasing (x:xs) = if x < head xs then increasing xs else False

decimate xs  = select (/=0) (map (`mod` 10) [1..length xs])

select f xs ys = map snd $ filter (f . fst) $  zip xs ys

encipher xs ys zs = map encipherElem zs where
  encipherElem z = head $ select (z==) xs ys

prefixSum zs = prefixSumAcc 0 zs where
  prefixSumAcc _ [] = []
  prefixSumAcc acc (x:xs) = (acc + x):prefixSumAcc (acc + x) xs

numbers xs = numbersAcc 0 xs where
  numbersAcc acc [] = acc
  numbersAcc acc (y:ys) = numbersAcc (acc*10 + y) ys

-- Имея список целых чисел, посчитать сумму четных чисел
sumEven :: (Eq a, Integral a, Num a) => [a] -> a

sumEven xs = sum $ select (==0) (map (`mod` 2) [1..length xs]) xs


sumEven'' = sum . filter ((==0) . (`mod` 2))

sumEven' xs = sum [x | x <- xs, x `mod` 2 == 0]

--Суммирует все натуральные числа меньше 1000, которые кратны 3 и 5.

sumI xs = sum [x | x <- [1..999], x `mod` 3 == 0 && x `mod` 5 ==0]

--Находит самое маленькое число, которое делиться на числа от 1 до 20 без остатка.

number = head [x | x <- [1..], and $ map (f x) [1..5]]
  where f x y = x `mod` y == 0
