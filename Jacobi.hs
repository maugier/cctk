module Jacobi where


jacobi :: Integer -> Integer -> Int
jacobi 1 n = 1
jacobi a n | a > n        = jacobi (a `mod` n) n
           | gcd a n /= 1 = 0
           | a == 2       = case n `mod` 8 of
                                1 -> 1
                                3 -> -1
                                5 -> -1
                                7 -> 1
           | even a = jacobi 2 n * jacobi (a `div` 2) n
           | otherwise = jacobi n a * if (n `mod` 4 == 3) && (a `mod` 4) == 3 then -1 else 1
