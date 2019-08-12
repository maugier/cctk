module RC4 where

import Data.Array.Unboxed
import qualified RC4.ST
import RC4.ST (Key, stream)
import Data.Array.ST (freeze)
import qualified Control.Monad.ST as ST
import Data.Word

ksa :: Key -> Array Word8 Word8
ksa k = ST.runST $ do
    (a, _, _) <- RC4.ST.setup k
    freeze a
