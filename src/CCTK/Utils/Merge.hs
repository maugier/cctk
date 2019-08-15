module CCTK.Utils.Merge (
    merge, 
    merge'
) where

import Data.List (sortBy)
import Data.Ord (comparing)

{- Merge sorted lists -}

merge :: Ord k => [(k,a)] -> [(k,b)] -> [(k,a,b)]
merge aa@((ka,a):as) bb@((kb,b):bs) = case compare ka kb of
    LT -> merge as bb
    GT -> merge aa bs
    EQ -> (ka,a,b) : merge as bs
merge _ _ = []

-- Try to factor this code. See how the typechecker bites you in the ass.
merge' :: Ord k => [(k,a)] -> [(k,b)] -> [(k,a,b)]
merge' as bs = merge (sortBy (comparing fst) as) (sortBy (comparing fst) bs)
