addMinusTwo :: (Num a) => a -> a
addMinusTwo = \x -> x-2

len :: (Num a,Floating a) => (a,a) -> a
len = \(x,y) -> sqrt (x*x + y*y)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumSqrt' :: Num a => [a] -> a
sumSqrt' = sum . map (^2)

sumWith :: Num a => (a->a) -> [a] -> a
sumWith f list = sum (map f list)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f list = prod' (map f list)
