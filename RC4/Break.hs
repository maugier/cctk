module RC4.Break where

import Data.Array.Unboxed
import Data.Tuple
import Data.Word
import RC4.ST

invertArray :: Ix a => Array a a -> Array a a
invertArray a = array (bounds a) (map swap (assocs a))

arrayLookup :: (Ix i, Eq e) => Array i e -> e -> i 
arrayLookup a e = head [ i | (i,e') <- assocs a, e' == e ]

backtrackState :: Array Word8 Word8 -> [(Array Word8 Word8, Word8)]
backtrackState a = scanr step (a,0) (uncurry enumFromTo . bounds $ a) where
    step i (a',_) = (flipped, j) where
        j = arrayLookup a' i
        flipped = a' // [(i, a' ! j), (j, a' ! i)]


keyFromState :: Array Word8 Word8 -> Key
keyFromState a = ks where
    (as, js) = unzip (backtrackState a) 
    jds = zipWith (-) (init js) (0:js)
    ks = zipWith (-) jds [0..255]
    
