import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reversed
    where
        reversed = reverse . sort $ xs