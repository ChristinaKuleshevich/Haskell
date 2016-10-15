--Main> myTakeWhile (/= ’ ’) "This is practice."
--"This"
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) = if f x then x:myTakeWhile f xs  else []

--Main> mySpan (/= ’ ’) "This is practice."
--("This"," is practice.")
mySpan f xs = splitAt n xs where n = length $ myTakeWhile f xs

--Int -> [a] -> ([a],[a]) 
mySplitAt n xs = mySplitAt' n ([], xs) where
  mySplitAt' 0 (left, right) = (reverse left, right)
  mySplitAt' _ (left, []) = (left, [])
  mySplitAt' n1 (left, x:right) = mySplitAt' (n1-1) (x:left, right)

mySplitAt' n xs = (myTake n xs, myDrop n xs)

myTake n xs = if n<=0 || null xs then [] else head xs: myTake (n-1) (tail xs)

myDrop n xs = if n<=0 || null xs then xs else myDrop (n-1) (tail xs)
myDrop' n xs
  | n<=0 || null xs = xs
  | otherwise       = myDrop' (n-1) (tail xs)




--combinations3 :: (Ord a) => [a] -> [[a]]
--combinations3 "ABCDE"
--["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]

--combinations3 (x:xs) 

increasing [] = True
increasing [x] = True
increasing (x:xs) = if x < head xs then increasing xs else False

select f xs ys = map snd $ filter (f . fst) $ zip xs ys

--encipher xs ys zs = map fst $ filter ((z==) . snd) $ zip xs ys
