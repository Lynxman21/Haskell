doubleElements :: Num a => [a] -> [a]
doubleElements [] = []
doubleElements (x:xs) = 2 * x : doubleElements xs

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
