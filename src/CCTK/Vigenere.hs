module Vigenere where

import Alphabetical
import Data.Either
import Likelihood
import Partial
import Util

key :: String -> Letters
key = cycle . rights . letters

encrypt :: Letters -> Letters -> Letters
encrypt = zipWith (+) . cycle

decrypt :: Letters -> Letters -> Letters
decrypt = zipWith (flip (-)) . cycle 

