module CCTK.RandomNatural where

import System.Random
import GHC.Natural

instance Random Natural where
    randomR (a,b) g = let (x,g') = randomR (toInteger a, toInteger b) g in (fromInteger x, g')
    random = error "No reasonable default range for Natural"
