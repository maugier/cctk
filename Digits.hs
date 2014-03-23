module Digits (fromDigits
              ,toDigits) where

import Data.List

frac x = x - fromIntegral (floor x)

fromDigits :: Integral a => a -> [a] -> a
fromDigits base = sum . zipWith (*) (iterate (*base) 1)

toDigits :: Integral a => a -> a -> [a]
toDigits base = unfoldr f where
    f 0 = Nothing
    f x = Just (x `mod` base, x `div` base)

