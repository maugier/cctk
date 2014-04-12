module Digits (fromDigits
              ,toDigits
	      ,expand) where

import Data.List

frac x = x - fromIntegral (floor x)

fromDigits :: Integral a => a -> [a] -> a
fromDigits base = sum . zipWith (*) (iterate (*base) 1)

toDigits :: Integral a => a -> a -> [a]
toDigits base = unfoldr f where
    f 0 = Nothing
    f x = Just (x `mod` base, x `div` base)

expand n (x:xs)         = x : expand (n-1) xs
expand n [] | n < 1     = []
            | otherwise = replicate n 0 
