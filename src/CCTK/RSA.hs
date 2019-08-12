
module RSA where

import Primes

type Exponent = Integer
type Modulus  = Integer
type Prime    = Integer
type DecExponent = Integer
type Cleartext = Integer
type Ciphertext = Integer

type Key    = (Modulus, Exponent)
type Params = (Prime, Prime)

data PrivateKey = PrivateKey (Maybe Modulus) Exponent (Maybe Prime) (Maybe Prime) (Maybe DecExponent)
	deriving Show



public :: PrivateKey -> Key
public (PrivateKey (Just n) e _ _ _) = (n,e)
public p = public (computePrivate p)


computePrivate :: PrivateKey -> PrivateKey
computePrivate p@(PrivateKey (Just _) _ (Just _) (Just _) (Just _)) = p
computePrivate (PrivateKey Nothing e (Just p) (Just q) d) = computePrivate (PrivateKey (Just (p*q)) e (Just p) (Just q) d)
computePrivate (PrivateKey n e (Just p) (Just q) Nothing) = computePrivate (PrivateKey n e (Just p) (Just q) (Just d)) where
    phi      = (p-1)*(q-1)
    (_,d',1) = euclid phi e
    d        = if d' > 0 then d' else d' + phi


encrypt :: Key -> Cleartext -> Ciphertext
encrypt (n,e) m = emod m e n

decrypt :: PrivateKey -> Ciphertext -> Cleartext
decrypt (PrivateKey (Just n) _ _ _ (Just d)) c      = emod c d n
decrypt p c = decrypt (computePrivate p) c



