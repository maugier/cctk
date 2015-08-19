module Wiener where

import CFrac
import GHC.Real

-- wiener n e = (phi,d) such that ed = 1 mod phi(n)
wiener :: Integer -> Integer -> Integer
wiener n e = head [ phi | (k :% d) <- convergents (e % n)
                        , k /= 0
                        , let phi = (e*d - 1) `div` k
                        , (e*d) `mod` phi == 1 ]
