isPalindrom :: [Char] -> Bool
isPalindrom arr = arr == reverse arr

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx ind arr = head (drop ind arr)

capitalize :: [Char] -> [Char]
capitalize slowo = if fromEnum (head slowo) >= 97 && fromEnum (head slowo) <= 122
    then toEnum (fromEnum (head slowo) - 32) : tail slowo
    else slowo