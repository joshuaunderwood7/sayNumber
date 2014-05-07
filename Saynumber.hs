module SayNumber where

sayNumber 0 = "zero"
sayNumber x = say x

sayHundreds x
    | x `mod` 100 == 0 = say (x `div` 100) ++ "-hundred"
    | otherwise        = say (x `div` 100) ++ "-hundred and " ++ say (x `mod` 100)
sayThousands x
    | x `mod` 1000 == 0   = say (x `div` 1000) ++ " thousand"
    | x `mod` 1000 >= 100 = say (x `div` 1000) ++ " thousand, " ++ say (x `mod` 1000)
    | otherwise      = say (x `div` 1000) ++ " thousand, and " ++ say (x `mod` 1000)

sayTens x 
    | x == 20           = "tewnty"
    | x `div` 10 == 2   = "tewnty-" ++ say (x `mod` 10)
    | x == 30           = "thirty"
    | x `div` 10 == 3   = "thirty-" ++ say (x `mod` 10)
    | x == 40           = "fourty"
    | x `div` 10 == 4   = "fourty-" ++ say (x `mod` 10)
    | x == 50           = "fifty"
    | x `div` 10 == 5   = "fifty-" ++ say (x `mod` 10)
    | x == 60           = "sixty"
    | x `div` 10 == 6   = "sixty-" ++ say (x `mod` 10)
    | x == 70           = "seventy"
    | x `div` 10 == 7   = "seventy-" ++ say (x `mod` 10)
    | x == 80           = "eighty"
    | x `div` 10 == 8   = "eighty-" ++ say (x `mod` 10)
    | x == 90           = "ninety"
    | x `div` 10 == 9   = "ninety-" ++ say (x `mod` 10)


say x
    | x == 0 = ""
    | x `elem` [14, 16, 17, 18, 19] = say (x `mod` 10) ++ "teen"
    | x < 100 && x >= 20 = sayTens x
    | x < 1000 && x >= 100 = sayHundreds x
    | x < 1000000 && x >= 1000 = sayThousands x


say 15 = "fifteen"
say 13 = "thirteen"
say 12 = "twelve"
say 11 = "eleven"
say 10 = "ten"
say 9 = "nine"
say 8 = "eight"
say 7 = "seven"
say 6 = "six"
say 5 = "five"
say 4 = "four"
say 3 = "three"
say 2 = "two"
say 1 = "one"

--main = do
--    mapM print $ map sayNumber [11001, 11002 .. 11110]
