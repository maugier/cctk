module CFrac where

import Data.List

cfrac :: (Integral b, RealFrac a) => a -> [b]
cfrac x = let (q,r) = properFraction x in q : case r of
    0 -> []
    _ -> cfrac (1 / r)

unCfrac :: (Integral b, Fractional a) => [b] -> a
unCfrac [] = error "Empty continuous fraction"
unCfrac [x] = fromIntegral x
unCfrac (x:xs) = fromIntegral x + 1 / unCfrac xs

convergents :: RealFrac a => a -> [a]
convergents = map unCfrac . tail . inits . cfrac
