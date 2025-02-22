{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
sgn :: Int -> Int
sgn n = if n < 0
    then -1 
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt a = if a < 0
    then -a 
    else a

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x > y
    then y
    else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) = if x <= y && x <= z
    then x
    else if y <= z
        then y
        else
            z

toUpper :: Char -> Char
toUpper letter = let n = fromEnum letter in
    if n <= 90 && n >= 65
        then letter
        else toEnum (n-32)
    
