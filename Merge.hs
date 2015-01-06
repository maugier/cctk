module Merge where

{- Merge sorted lists -}

merge :: Ord k => [(k,a)] -> [(k,b)] -> [(k,a,b)]
merge aa@((ka,a):as) bb@((kb,b):bs) = case compare ka kb of
	LT -> merge as bb
	GT -> merge aa bs
	EQ -> (ka,a,b) : merge as bs
merge _ _ = []
