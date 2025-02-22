not :: Bool -> Bool
not True = False
not False = True

isItAnswer :: String -> Bool
isItAnswer "Love"  = True
isItAnswer _      = False

alternative :: (Bool, Bool) -> Bool
alternative (x,y) = if x == False && y == False
    then False
    else True