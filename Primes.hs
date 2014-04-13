module Primes (primes, isPrime, divisors, factors, divides, phi, euclid, emod, introot, fermat) where

import Data.List (group)
import Control.Monad (msum)

-- Get primes by trial division

divides 1 _ = False
divides a b = b `mod` a == 0

cands :: Integral a => [a]
cands = concat [[k,k+2] | k <- iterate (+6) 5 ]

primes :: Integral a => [a]
primes = 2 : 3 : isPrime `filter` cands

isPrime 1 = False
isPrime 2 = True    -- needed because it would match the divides case below
isPrime p = sieve primes where
            sieve [] = True
            sieve (t:ts)  | t `divides` p = False
                          | t*t > p       = True
                          | otherwise     = sieve ts

square x = x*x

isPrime2 1 = False
isPrime2 2 = True
isPrime2 p = sieve primes where
	sieve = all (not . (`divides` p)) . takeWhile ((<=p).square)

divisors n = [ k | k <- [1..(n-1)], k `divides` n ]

factors n = try n primes where
	try 1 _ = []
	try n (p:ps)   | p `divides` n   = p : try (n `div` p) (p:ps)
		       | p*p > n         = [n]
	               | otherwise       = try n ps


phi :: (Integral a) => a -> a
phi   = product . map phiprime . groupcount . factors where
        groupcount xs = [ (head l, length l) | l <- group xs ]
        phiprime (p,k) = (p-1)*p^(k-1)

-- euclid a b = (i,j,k) | a*i + b*j = k
euclid a b 
  | b > a = let (j,i,k) = euclid b a in (i,j,k)
  | otherwise = euclid' (1,0,a) (0,1,b) where
      euclid' p@(i,j,k) q@(i',j',k') 
	| k' == 0   = p
	| otherwise = let f = k `div` k'
		in euclid' q (i-i'*f, j-j'*f, k-k'*f)

-- emod a e n = (a^e `mod` n)
emod _ 0 _ = 1
emod a e n | odd e     = (a * square (emod a (e `div` 2) n)) `mod` n
           | otherwise = square (emod a (e `div` 2) n) `mod` n

newton f t a b | a == b = Left a
	       | otherwise = let mid = (a + b) `div` 2 in
			case compare (f mid) t of
				EQ -> Right mid
				GT -> newton f t a mid
				LT -> newton f t (mid+1) b

-- Fermat factoring

introot x = newton square x 1 (x+1)

right = either (const Nothing) Just

fermat n effort skip = do
	let start = either id id (introot n)
	b <- msum [ right . introot $ a*a - n | a <- take effort (drop skip [start..]) ]
	let Right a = introot (b*b + n)
	return (a+b)

-- Test suite

dumbprimes = [k | k <- [2..], naiveprime k]

naiveprime k = (not . (`divides` k)) `all` [2..(k-1)] 

testprimes n = take n primes == take n dumbprimes
