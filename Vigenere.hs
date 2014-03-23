module Vigenere where

import Alphabetical

encrypt :: Letters -> Letters -> Letters
encrypt = zipWith (+) . cycle

decrypt :: Letters -> Letters -> Letters
decrypt = zipWith (flip (-)) . cycle 

