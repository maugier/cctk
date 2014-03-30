
module RSA where

type Exponent = Integer
type Modulus  = Integer
type Prime    = Integer
type Cleartext = Integer
type Ciphertext = Integer

type Key    = (Modulus, Exponent)
type Params = (Prime, Prime)

data PrivateKey = Private Key | Params Key Params





encrypt :: Key -> Cleartext -> Ciphertext
encrypt (n,e) m = emod m e n

decrypt :: PrivateKey -> Ciphertext -> Cleartext
decrypt (Private (n,d)) c      = emod c d n
decrypt (Params (n,e) (p,q)) c = decrypt (Private (n,d `mod` phi)) c where
    phi     = (p-1)*(q-1)
    (_,d,1) = egcd phi e

square x = x*x

emod _ 0 _ = 1
emod a e n | e < 0 = error $ "emod cannot compute power < 0: " ++ show e 
           | odd e = (a * emod a (e-1) n) `mod` n
           | otherwise = square (emod a (e `div` 2) n) `mod` n


egcd a b = egcd' (1,0,a) (0,1,b) where
    egcd' r (_,_,0) = r
    egcd' (x1,y1,g1) r2@(x2,y2,g2) = egcd' r2 (x3,y3,g3) where
        m = g1 `div` g2
        g3 = g1 - m*g2
        x3 = x1 - m*x2
        y3 = y1 - m*y2


