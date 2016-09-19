import Prelude



reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


reverse'' xs = myReverse xs []
  where myReverse []     acc = acc
        myReverse (x:xs) acc = myReverse xs (x:acc)


sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' ys = if null' ys then 0 else head' ys + sum'' (tail' ys)

             
product' [] = 1
product' (x:xs) = x * product' xs

head' []     = error "Empty list"
head' (x:_) = x


tail' [] = error "Empty list"
tail' (_:xs) = xs



length' [] = 0
length' (x:xs) = 1 + length' xs



null' [] = True
null' _  = False


elem' _ [] = False
elem' y (x:xs) = if y == x then True else elem' y xs

elem'' _ [] = False
elem'' y (x:xs) = (y == x) || elem'' y xs

notElem' x ys = not (elem' x ys)




-- | Same as (++)
append as bs = myAppend (reverse' as) bs where
  myAppend [] ys = ys
  myAppend (x:xs) ys = myAppend xs (x:ys)


id' x = x


const' x y = x


flip' f x y = f y x


lookup' _ [] = Nothing
lookup' y (x:xs) = if y == fst x then Just (snd x) else lookup' y xs  

zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []
                                                        
main :: IO ()
main = do
  return ()


names = ["sasha", "masha", "pasha", "dasha", "kasha"]

uncurry' f (x, y) = f x y


map' _ [] = []
map' f (x:xs) = f x : map' f xs
