sumWith :: Num a => (t -> a) -> [t] -> a
sumWith g [] = 0
sumWith g (x:xs) = g x + sumWith g xs

prodWith :: Num a => (t -> a) -> [t] -> a
prodWith g [] = 1
prodWith g (x:xs) = g x * sumWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
    where
        go acc g [] = acc
        go acc g (x:xs) = go (g x + acc) g xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

sumWith'' :: Num b => (a -> b) -> [a] -> b
sumWith'' g = foldl' (\acc x -> g x + acc) 0