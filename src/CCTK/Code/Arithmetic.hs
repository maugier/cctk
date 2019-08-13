module CCTK.Code.Arithmetic (
    encode,
    decode,
    encodeFixed,
    decodeFixed
) where

import Control.Applicative
import Data.Array as A
import Data.List (scanl')
import qualified Data.Map.Strict as M


encodeFixed :: [a] -> Int -> Integer -> [a]
encodeFixed code = go [] where
    go acc 0 _ = acc
    go acc l x = let (q,r) = x `quotRem` size' in go ((cache ! fromInteger r) : acc) (l-1) q
    cache = listArray (0, size-1) code
    size = length code
    size' = toInteger size

decodeFixed :: Ord a => [a] -> [a] -> Maybe Integer
decodeFixed code = go 0 where
    go acc [] = Just acc
    go acc (x:xs) = case M.lookup x cache of
        Nothing -> Nothing
        Just v  -> go (acc*size + v) xs

    cache = M.fromList (zip code [0..])
    size = toInteger (length code)

encode :: [a] -> Integer -> [a]
encode code x = encodeFixed code level (x - base) where
    (base, level) = last . takeWhile ((x >=) . fst) $ zip sums [0..]
    sums = scanl' (+) 0 powers
    powers = iterate (size*) 1
    size = toInteger (length code)


 
decode :: Ord a => [a] -> [a] -> Maybe Integer
decode code xs = (base+) <$> decodeFixed code xs where
    size = toInteger (length code)
    level = toInteger (length xs)
    base = geosum size level

geosum 1 n = fromIntegral n
geosum r n = (r^n - 1) `quot` (r - 1)
