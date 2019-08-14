module CCTK.RainbowTable (
    Table(),
    build,
    build1,
    build',
    crack,
    shape,
    effectiveness,
) where

import Control.Parallel.Strategies
import Control.Monad.Random
import Data.List (foldl', tails, unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Serialize
import Data.ByteString.Lazy as LBS (readFile, writeFile)

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
build' h rs = Table h rs . M.fromList . map (\c -> (chain' c, c)) where
    chain' = h . chain h rs

-- |- Iteratively build a table from a single seed. The first reducer is used to generate chains.
build1 :: Ord h => (c -> h) -> [h -> c] -> Int -> c -> Table h c
build1 h (r:rs) rows = Table h rs . M.fromList . take rows . unfoldr step where
    step c = let y = (h . chain h rs) c in Just ((y,c), r y)


-- |- Build a table of the desired size, using the standard RNG
build :: (RandomGen g, Ord h, Random c) => (c -> h) -> [h -> c] -> Int -> g -> Table h c
build h rs rows = build' h rs . take rows . randoms


effectiveness :: (Ord h, Random c) => Table h c -> IO Int
effectiveness t = measure 100 0 where
    measure 0 n = return n
    measure k n = do
        x <- getRandom
        case crack t (hash t x) of
            Just _ -> measure (k-1) (n+1)
            _      -> measure (k-1) n


save :: (Serialize h, Ord h, Serialize c) => FilePath -> Table h c -> IO ()
save path = LBS.writeFile path . encodeLazy . table


load :: (Serialize h, Ord h, Serialize c) => (c -> h) -> [h -> c] -> FilePath -> IO (Either String (Table h c))
load h rs file = load' . decodeLazy <$> LBS.readFile file where
    load' (Left e) = Left $ "Cannot decode: " ++ e
    load' (Right t) =
        case M.lookupMin t of
            Nothing -> Left "Loaded an empty table"
            Just (y, c)
                | y == (h . chain h rs) c -> Right $ Table h rs t
                | otherwise -> Left "Incorrect algorithms for table"
