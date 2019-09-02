-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = x : []
doubleEveryOtherFromLeft (x:y:xs) = x : 2 * y : doubleEveryOtherFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromLeft (reverse xs))

-- Exercise 3
sumDigitsInteger :: Integer -> Integer
sumDigitsInteger n
  | n == 0    = 0
  | otherwise = sumDigitsInteger (n `div` 10) + (n `mod` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsInteger x + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
