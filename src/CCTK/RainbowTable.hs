module CCTK.RainbowTable (
    Table(),
    build,
    crack
) where

import CCTK.Digits

import Data.List (foldl', tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

data Table h c = Table {
    hash   :: c -> h,
    reduce :: [h -> c],
    table  :: !(Map h c)
}

{-
c0 -> h0 -A> c1 -> h1 -B> c2 -> h2

build : c0 -> h.chain $ c0

attack: h0 -> h0, h1, h2

attack h0 [] = h0
attack h0 (r:rs) = attack (h (r h0)) rs

attack = foldl' (flip $\r -> (h.r)))


-} 

chain :: (c -> h) -> [h -> c] -> c -> c
chain h = flip (foldl' (flip (.h)))

attack :: (c -> h) -> [h -> c] -> h -> h
attack h = flip (foldl' (flip (h.)))


crack :: Ord h => Table h c -> h -> Maybe c
crack (Table h rs t) x =
    getFirst $ mconcat [ First .Just $ chain h (take i rs) c 
                       | (i,rs') <- zip [0..] (tails rs)
                       , let y = (attack h rs') x
                       , let (Just c) = M.lookup y t ]

build :: Ord h => (c -> h) -> Int -> c -> [h -> c] -> Table h c
build h rows c (r:rs) = Table h rs (tab c M.empty) where
    tab c t | M.size t >= rows = t
            | otherwise        = let y = h (chain h rs c) in tab (r y) (M.insert y c t)
