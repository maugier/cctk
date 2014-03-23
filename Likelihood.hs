module Likelihood (Histogram, histogram, score, histFromFile) where

import Data.Map (Map, fromListWith, (!))
import qualified Data.ByteString.Lazy as B

type Histogram k = Map k Int


histogram :: Ord k => [k] -> Histogram k
histogram = fromListWith (+) . map (\k -> (k,1))


score :: Ord k => Histogram k -> [k] -> Double
score m = sum . map (negate . log . fromIntegral . (m !))

histFromFile f = B.readFile f >>= return . histogram . B.unpack
