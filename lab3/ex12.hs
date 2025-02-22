import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
    map capitalize . filter (\x -> length x > 1) $ words s