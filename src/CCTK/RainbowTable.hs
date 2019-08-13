module CCTK.RainbowTable (
    Table(),
    build,
    build1,
    build',
    crack,
    shape,
    effectiveness,
) where

import CCTK.Digits

import Control.Monad.Random

import Data.List (foldl', tails, unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)

import System.Random

-- |- A Rainbow table for attacking a hash function of type `h -> c`
data Table h c = Table {
    hash   :: c -> h,
    reduce :: [h -> c],
    table  :: !(Map h c)
}

chain :: (c -> h) -> [h -> c] -> c -> c
chain h = flip (foldl' (flip (.h)))

attack :: (c -> h) -> [h -> c] -> h -> h
attack h = flip (foldl' (flip (h.)))

-- |- Check the shape of the table (colors, length)
shape :: Table a b -> (Int,Int)
shape (Table _ rs t) = (length rs + 1, M.size t)

-- |- Attempt to crack a hash using the precomputed table
crack :: Ord h => Table h c -> h -> Maybe c
crack (Table h rs t) x =
    listToMaybe [ c'
                | (i,rs') <- zip [0..] (tails rs)
                , let y = (attack h rs') x
                , Just c <- [M.lookup y t]
                , let c' = chain h (take i rs) c
                , h c' == x ]

-- |- Deterministically build a table for a given hash function.
-- Takes a hash function to attack, a list of reducers, and a list of chain seeds.
--
-- If the list of reducers is empty, the table is equivalent to a full dictionary.
build' :: Ord h => (c -> h) -> [h -> c] -> [c] -> Table h c
build' h rs cs = Table h rs (M.fromList [ ((h . chain h rs) c ,c ) | c <- cs ])

-- |- Iteratively build a table from a single seed.
build1 :: Ord h => (c -> h) -> [h -> c] -> Int -> c -> Table h c
build1 h (r:rs) rows = Table h rs . M.fromList . take rows . unfoldr step where
    step c = let y = (h . chain h rs) c in Just ((y,c), r y)


-- |- Build a table of the desired size from random seeds
build :: (Ord h, Random c, MonadRandom m) => (c -> h) -> [h -> c] -> (Int, Int) -> m (Table h c)
build = undefined


effectiveness :: (Ord h, Random c) => Table h c -> IO Int
effectiveness t = measure 100 0 where
    measure 0 n = return n
    measure k n = do
        x <- getRandom
        case crack t (hash t x) of
            Just _ -> measure (k-1) (n+1)
            _      -> measure (k-1) n
