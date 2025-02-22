isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all (uncurry (<=)) (zip xs (tail xs)) --tworzy krotki z elementów zipa i sprawdza warunek dla wszystkich

everySecond :: [t] -> [t]
everySecond xs = map snd $ filter (odd . fst) $ zip [1..] xs