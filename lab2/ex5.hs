three :: [(Int,Int,Int)]
three = [(a,b,c) | a <- [1..100],b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime a = null [i | i <- [2..a-1], a `mod` i == 0]

howMuchPrimes :: Int
howMuchPrimes = length [x | x <- [2..10000], isPrime x]

primes :: Int -> [Int]
primes n = eratoSieve [x | x <- [2..],x<=n]
    where
        eratoSieve :: [Int] -> [Int]
        eratoSieve (p:xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

allEquals :: Eq a => [a] -> Bool
allEquals xs = all (== head xs) xs