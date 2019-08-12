module CCTK.ContinuedFraction (
    CFrac, 
    unCFrac, 
    fromCFrac, 
    toCFrac, 
    convergents, 
    cFracSqrt
) where

import Data.List (inits,unfoldr)
import Data.Ratio
import Math.NumberTheory.Powers.Squares

newtype CFrac a = CFrac { unCFrac :: [a] }
    deriving (Show, Eq, Ord)

fromCFrac :: (RealFrac a, Integral b) => CFrac b -> a
fromCFrac (CFrac []) = 0
fromCFrac (CFrac [x]) = fromIntegral x
fromCFrac (CFrac (x:xs)) = fromIntegral x + 1 / fromCFrac (CFrac xs)

toCFrac :: (RealFrac a, Integral b) => a -> CFrac b
toCFrac x = let
    xf = floor x 
    xr = x - fromIntegral xf
    in case xr of
        0 -> CFrac [xf]
        n -> CFrac (xf : unCFrac (toCFrac (1/xr)))
    
convergents :: (Integral a) => CFrac a -> [Ratio a]
convergents = map (\(h,_,k,_) -> h % k) . tail . scanl (\(h,h',k,k') a -> (a*h+h', h, a*k+k', k)) (1,0,0,1) . unCFrac 


cFracSqrt :: Integer -> CFrac Integer
cFracSqrt n = CFrac [ a | (a,_,_) <- iterate apq (nfloor, 0, 1) ] where
    nfloor = integerSquareRoot n
    apq (a,p,q) = let p' = a*q - p
                      q' = (n - p'*p') `div` q
                      a' = (nfloor + p') `div` q'
                   in (a',p',q')

