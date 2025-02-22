sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where 
        intFunc x = x ^ 5

fractrial :: Int -> Int
fractrial 0 = 1
fractrial n = n * fractrial (n-1)

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = \x -> sum [x^k / fromIntegral (fractrial k) | k <- [0..n]]

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x+h) - f x) / h

