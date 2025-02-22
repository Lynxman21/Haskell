fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' xs | null xs   = 0
        | otherwise = head xs + sum' (tail xs)

sum2 :: Num a => [a] -> a
sum2 xs = if null xs
    then 0
    else head xs + sum2 (tail xs)

prod' :: Num a => [a] -> a
prod' xs =
    if null xs
        then 1
        else head xs * prod' (tail xs)

len :: Num a => [a] -> a
len xs =
    if null xs
        then 0
        else 1 + len (tail xs)

or' :: [Bool] -> Bool
or' xs = if null xs
    then False
    else
        if head xs == True
            then True
            else or' (tail xs)

and' :: [Bool] -> Bool
and' xs | null xs = True
        | not (head xs) = False
        | otherwise = and' (tail xs)

elem' :: Eq a => a -> [a] -> Bool
elem' n xs | null xs = False
           | head xs == n = True
           | otherwise = elem' n (tail xs)

doubleAll :: Num a => [a] -> [a]
doubleAll xs | null xs = []
             | otherwise = (2*head xs) : doubleAll (tail xs)

doubleAll2 :: Num a => [a] -> [a]
doubleAll2 xs = [2*x | x <- xs]

sum2' :: Num a => [a] -> a
sum2' xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (x+acc) xs

sum3' :: Num a => [a] -> a
sum3' = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (x+acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where loop sc [] = sc
          loop sc (x:xs) = loop (x*sc) xs