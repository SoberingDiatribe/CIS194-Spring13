{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = lastDigit x : (toDigitsRev . dropLastDigit) x

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2 -----------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:zs) = x : 2 * y : doubleEveryOther zs
doubleEveryOther xs       = xs

-- Exercise 3 -----------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigitsRev) 0

-- Exercise 4 -----------------------------------------

validate :: Integer -> Bool
validate = (0 ==) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

-- Exercise 5 -----------------------------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start temporary end
  | n <= 0 = []
  | n == 1 = [(start, end)]
  | otherwise = hanoi (n - 1) start end temporary ++ [(start, end)] ++ hanoi (n - 1) temporary start end
