module CCTK.RainbowTable.Reducers (
   arithmeticFixed
) where

import CCTK.RainbowTable
import CCTK.Digits
import CCTK.Code.Arithmetic
import Data.Array
import Data.Word


arithmeticFixed :: [a] -> Int -> Integer -> [Word8] -> [a]
arithmeticFixed code len swirl = encodeFixed code len . fromDigits 256 . map ((swirl +) . toInteger)

