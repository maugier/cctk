
module CCTK.RSA (
    encrypt,
    decrypt,
    genParameters,
    genKeys,
    makeKeys,
) where

import CCTK.RandomNatural
import Control.Applicative
import Control.Monad.Random
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli
import GHC.Natural

type Modulus = Natural
type Exponent = Integer

type Parameters = (Prime Natural, Prime Natural)

type Public = (Exponent, Modulus)
type Private = (Exponent, Modulus)


encrypt :: Public -> Integer -> Integer
encrypt (e,n) m = case m `modulo` n of SomeMod m -> getVal (powMod m e)

decrypt :: Private -> Integer -> Integer
decrypt = encrypt

genParameters :: MonadRandom m => Int -> m Parameters
genParameters bits = do
    p <- randomPrime bits
    q <- randomPrime bits
    return (p,q)


groupOrder :: Parameters -> Natural
groupOrder (p, q) = (unPrime p - 1)*(unPrime q - 1)

randomPrime :: MonadRandom m => Int ->  m (Prime Natural)
randomPrime bits = go where
    range = (2^(bits-1), 2^bits-1) 
    go = do
        p <- getRandomR range
        case isPrime p of
            Just p' -> return p'
            Nothing -> go

makeKeys :: Parameters -> Exponent -> Maybe (Public, Private)
makeKeys pq@(p,q) e =
    case e `modulo` groupOrder pq of
        SomeMod e' -> fromD . getVal <$> invertMod e'
    where
        n = unPrime p * unPrime q
        public = (e,n)
        fromD d = (private, public) where
            private = (d,n)

genKeys :: MonadRandom m => Int -> Exponent -> m (Public, Private)
genKeys bits e = do
    param <- genParameters bits
    case makeKeys param e of
        Just pair -> return pair
        Nothing -> genKeys bits e
