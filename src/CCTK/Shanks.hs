{- |
   Module: CCTK.Shanks

   David Shanks' baby-steps giant-steps algorithm
   computes the discrete logarithm in any group

-}

module CCTK.Shanks where

import CCTK.Group
import CCTK.Merge (merge')
import Math.NumberTheory.Powers.Squares

shanks :: (Group g, Ord g) => Integer -> g -> g -> Maybe Integer
shanks k g y = let
        giantStep = integerSquareRoot k + 1
        giant = g |^| negate giantStep
        babies = zip (iterate (<> g)     i) [0..giantStep]
        giants = zip (iterate (<> giant) y) [0..giantStep]
    in case merge' babies giants of
        [] -> Nothing
        (_,a,b):_ -> Just $ a + giantStep * b
