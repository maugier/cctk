module Combinatorics where

pascal = iterate (\l -> zipWith (+) (0:l) (l++[0])) [1]

facs = scanl (*) 1 [1..]

fac = (facs !!)

choose n k | k > n = 0
           | otherwise = product [k+1..n] `div` product [2..n-k]

m1p x | even x = 1
      | otherwise = -1

stirling n k = sum [ m1p (k-j) * choose k j * j^n | j <- [1..k] ] `div` fac k
