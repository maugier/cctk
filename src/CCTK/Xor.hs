module Xor where

import Data.Bits
import Data.List (transpose)
import qualified Data.Map as M
import Data.Word
import Digits
import Likelihood
import Util

encrypt :: Bits a => [a] -> [a] -> [a]
encrypt = zipWith xor . cycle

decrypt :: Bits a => [a] -> [a] -> [a]
decrypt = encrypt

bytePairDistribution = M.fromListWith (+) [ (x `xor` y, px*py) | (x,px) <- h, (y,py) <- h ] where
    h = M.toList defaultBytes

breakOneKey :: Histogram Word8 -> [Word8] -> Word8
breakOneKey h cs = best $ M.fromListWith (+) [ (k, product [ h M.! (k `xor` c) | c <- cs ]) | k <- [0..255] ]


breakNKey :: Histogram Word8 -> Int -> [Word8] -> [Word8]
breakNKey h n = map (breakOneKey h) . transpose . chunk n
