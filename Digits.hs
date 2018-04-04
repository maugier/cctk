module Digits (fromDigits
              ,toDigits
              ,expand) where

import Data.List

frac x = x - fromIntegral (floor x)

fromDigits :: (Integral a, Num b) => b -> [a] -> b
fromDigits base = sum . zipWith (*) (iterate (*base) 1) . map fromIntegral

toDigits :: (Integral a, Num b) => a -> a -> [b]
toDigits base = map fromIntegral . unfoldr f where
    f 0 = Nothing
    f x = Just (x `mod` base, x `div` base)

expand n (x:xs)         = x : expand (n-1) xs
expand n [] | n < 1     = []
            | otherwise = replicate n 0 
