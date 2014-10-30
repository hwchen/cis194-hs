-- Haskell UPenn Course Hwrk 1

-- 1. CC Validation

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x 
    | 0 <= x && x <= 9 = [x]
    | otherwise = toDigits(div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
    | 0 <= x && x <= 9 = [x]
    | otherwise = (mod x 10) : toDigitsRev(div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther all@(x:y:zs)
    | mod (length all) 2 == 0 = 2*x : y : (doubleEveryOther zs)
    | mod (length all) 2 /= 0 = x : 2*y : (doubleEveryOther zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] 
    | 0 <= x && x <= 9 = x
    | otherwise = sumDigits (toDigits x)
sumDigits (x:xs) 
    | 0 <= x && x <= 9 = x + (sumDigits xs)
    | otherwise = sumDigits (toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate x
    | n == 0 = True
    | n /= 0 = False
    where n = mod (sumDigits (doubleEveryOther (toDigits x))) 10
