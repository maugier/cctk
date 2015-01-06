{- David Shanks' baby-steps giant-steps algorithm 

   given g, y, elements of a group
   shanks k g y = x such that x * g = b, with x in [0..k]

-}

module Shanks where

import Group
import Merge (merge)

shanks :: (Group g, Ord g) => Integer -> g -> g -> Maybe Integer
shanks k g y = let
		giantStep = ceiling . sqrt $ (fromInteger k :: Double)
		giant = g |^| negate giantStep
		babies = zip (iterate (|*| g)     i) [0..]
		giants = zip (iterate (|*| giant) y) [0..]
	in case merge babies giants of
		[] -> Nothing
		(_,a,b):_ -> Just $ a + giantStep * b

