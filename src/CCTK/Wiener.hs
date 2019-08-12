module Wiener where

import CFrac
import Data.Maybe
import GHC.Real
import Math.NumberTheory.Powers.Squares

type Exponent = Integer
type Modulus = Integer

-- wiener n e = (p,q) such that p*q = n
wiener :: Modulus -> Exponent -> (Integer, Integer)
wiener n e = head [ (p,q) | (k :% d) <- convergents (e % n)
                        , k /= 0
                        , let (phi,r) = (e*d - 1) `divMod` k
                        , r == 0
                        , (e*d) `mod` phi == 1
                        , (p,q) <- maybeToList $ factorWithPhi n phi
                        , p*q == n
                        ]


factorWithPhi :: Integer -> Integer -> Maybe (Integer, Integer)
factorWithPhi n phi = do
    let b = n - phi + 1
    let c = n
    let d = b*b - 4*c
    dr <- exactSquareRoot d
    return ((b - dr) `div` 2, (b + dr) `div` 2)

