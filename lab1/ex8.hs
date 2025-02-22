negative :: Bool -> Bool
negative ans = case ans of
    True -> False
    False -> True

absInt :: Int -> Int
absInt n = case (n >= 0) of
    True -> n
    False -> -n

