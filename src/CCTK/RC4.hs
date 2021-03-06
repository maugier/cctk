{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module CCTK.RC4 (
    Key, State, setup, stream
) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Control.Monad.ST.Lazy as STL
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.STRef
import Data.Word
--import STOps

type Key = [Word8]

-- |RC4 Cipher state
data State s = State (STUArray s Word8 Word8) (STRef s Word8) (STRef s Word8)

aswap :: STUArray s Word8 Word8 -> Word8 -> Word8 -> ST s ()
aswap s i j = do
    x <- readArray s i
    y <- readArray s j
    writeArray s i y
    writeArray s j x

ksa :: Key -> ST s (STUArray s Word8 Word8)
ksa k = do
    s <- newListArray (0,255) [0..255]
    j <- newSTRef 0
    forM_ (zip [0..255] (cycle k)) $ \(i, k_i) -> do
        s_i <- readArray s i
        j += (s_i + k_i)
        readSTRef j >>= aswap s i 
    return s
        
-- |Initializes the cipher from a key
setup :: Key -> ST s (State s)
setup key = State <$> ksa key <*> newSTRef 0 <*> newSTRef 0


{-
dump :: State s -> ST s (Word8,Word8,UArray Word8 Word8)
dump (s,i,j) = do
    i' <- readSTRef i
    j' <- readSTRef j
    s' <- freeze s
    return (i',j',s')
-}

(+=) :: Num a => STRef s a -> a -> ST s ()
s += a = modifySTRef' s (a+)
    
step :: State s -> ST s Word8
step (State s i j) = do
    i += 1
    i' <- readSTRef i
    s_i <- readArray s i'
    j += s_i
    j' <- readSTRef j
    s_j <- readArray s j'
    aswap s i' j'
    readArray s (s_i + s_j)

-- |Lazy stream of bytes for the given key
stream :: Key -> [Word8]
stream k = STL.runST $ do
    s <- STL.strictToLazyST $ setup k
    sequence . repeat . STL.strictToLazyST $ step s
