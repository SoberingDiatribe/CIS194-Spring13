{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips x  = zipWith (takeEvery 1) [1 .. n] (replicate n x)
  where
    n = length x

takeEvery :: Int -> Int -> [a] -> [a]
takeEvery _ _ [] = []
takeEvery n m (x:xs)
  | n == m    = x : takeEvery 1 m xs
  | otherwise = takeEvery (n + 1) m xs

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:ds)
  | a < b && b > c = b : localMaxima (b:c:ds)
  | otherwise      = localMaxima (b:c:ds)
localMaxima _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram x =
  unlines $ transpose $ zipWith convert [0 ..] counts
  where
    counts = map (subtract 1 . length) $ (group . sort) ([0 .. 9] ++ x)
    mode = maximum counts
    convert :: Integer -> Int -> String
    convert index n = concat [replicate (mode - n) ' ', replicate n '*', "=", show index]
