module CCTK.RSA.Partial where

import Control.Arrow
import Control.Monad
import Data.Maybe
import CCTK.Digits
import CCTK.Partial
import CCTK.RSA

type PartialBits = Partial Int
type Pentuple a = (a,a,a,a,a)
type PartialKey = Pentuple PartialBits

hexDigit = (`elem` "0123456789abcdefABCDEF")

fromHex :: String -> Integer
fromHex = read . ("0x"++) . filter hexDigit

fromPartialHex :: String -> Partial Int
fromPartialHex = ((Partial . reverse) .) . concatMap $ \x -> case x of
    ' '            -> [Nothing,Nothing,Nothing,Nothing]
    c | hexDigit c -> map Just . reverse 
                      . expand 4 . toDigits 2 . fromIntegral . fromHex . (:[]) $ c
    _              -> [] 

fromPartialBits (Partial xs) = fmap (fromDigits 2) $ sequence xs 

fromBits = fromDigits 2 . map fromIntegral


e = 65537

n = fromHex "00:db:fa:bd:b1:49:5d:32:76:e7:62:6b:84:79:6e:9f:c2:0f:a1:3c:17:44:f1:0c:8c:3f:3e:3c:2c:60:40:c2:e7:f3:13:df:a3:d1:fe:10:d1:ae:57:7c:fe:ab:74:52:aa:53:10:2e:ef:7b:e0:09:9c:02:25:60:e5:7a:5c:30:d5:09:40:64:2d:1b:09:7d:d2:10:9a:e0:2f:2d:cf:f8:19:8c:d5:a3:95:fc:ac:42:66:10:78:48:b9:dd:63:c3:87:d2:53:8e:50:41:53:43:04:20:33:ea:09:c0:84:15:5e:65:2b:0f:06:23:40:d5:d4:71:7a:40:2a:9d:80:6a:6b"
--n = fromHex "00:ed:ab:2a:04:48:03:91:88:bd:27:0a:94:cc:e0:f8:55"

 
corruptedD = fromPartialHex " f:  :  : a: a:9 :e :  : 1: 2:  :  :e :  :1 :3 : 1:  :  :  : a:  :2 :  :  :  :  :  :  :  :9 : a:  :  :  :  :  : 5:c1: 0:b : 3: 2:0 :b0:  :c : f:  :f :  :d2:  :  : d:  :1 :  :3 :  :  :  :  :0 : 3:  :  : 5:c :  :3 :6 :  :a4:  :4 :  :  :8f:  :  :  :  : a:  : c:5f: 7: 6:  : 1:  : b:  : 5:  :84:0 :b : f: 3:  :  : 4: 6:  :  : 5:1 :  :d :  : f:  : c:  :  : 5:  :  :  :e :f4:b :4 :8e:  :  "
corruptedP = fromPartialHex "00:  :6 : 1:1 :  :b :0 : 2:c : b:2 :  : a:1 :c :  : 0:  :28:0 :  :cd:  : 8:  :  :20: c:  :  : 5:  :9 : c:3 :  :  : a:b :c :3 :  :  :  : f:  :  : f: 1: 1:b :  : c:f : a:  :a :  :  : a:38:  :6 :  "
corruptedQ = fromPartialHex "  :e :  :d :2 :6 : 7:  :33:  :46:  : 4:  :  :  :5 :  : 4:6 :  : 6:  : e:d :  :  : 9: e:1 :  :  :  :  :  :0 :  :  :  :c : 5:  :  :a :0 :6 :  :  :8 :e9:f : f:7 :5 : e:1 :  :  : 1:9 : 4:d :e9: 6:  "
corruptedDP = fromPartialHex " 9:d : 5:  :c :67:  : 9:  :  :  : d:  :  : 3: f:6 : 0:c :  :6 :ad:  :2 :d :d :  :  :0 :7 :  :5 : 6:  : 5:1 :f : d:  : 2:  :  : 2: 3:  :9 :  :  :  :  :67: 3:  :4 : 7:c0: 4:b :c :f :  :3 :b : 1"
corruptedDQ = fromPartialHex "1 : 9:47:8 :  :  :  : 3:  :  :  :6 :  :  :0 :e :e :8 :  :  :  :  : 1:c :74:  :  :d : 9:3 :5 : e:  : 2:  :7 : 2:c :  :  :  :  :5 :  : 8:  :  :c :  : 1:  :a :  : 9: 5:  : 3:  : e:c :  :  : 6:  "

target :: PartialKey
target = (corruptedP, corruptedQ, corruptedD, corruptedDP, corruptedDQ)

fromKnownInteger :: Integer -> PartialBits
fromKnownInteger = fromKnown . toDigits 2


breakK :: Integer -> Integer -> PartialBits -> [Integer]
breakK n e cd = [ k | k <- [1..(e-1)] , cd >?< msb l (fromKnownInteger (approx k)) ] where
    l = (length (fromPartial cd) `div` 2) + 2
    approx k = (k * (n+1) + 1) `div` e

k = 4695

breakKPQ n e k = [ kp | kp <- [1..(e-1)], (kp*kp - kp*c - k) `mod` e == 0 ] 
    where c = k*(n-1)+1

tau x = tau' x 0 where
    tau' x y | odd x     = y
             | otherwise = tau' (x `div` 2) (y+1)

exhaustBit = exhaust (0,1)

(>><) :: [Int] -> PartialBits -> [([Int],Integer)]
x >>< y = (extend (fromKnown x) >< y) >>= map (id &&& fromDigits 2) . exhaustBit

breakKey :: Integer -> Integer -> PartialKey -> [(Integer,Integer)]
breakKey n e (p,q,d,dp,dq) = do

    k <- breakK n e d

    (kp,kq) <- case breakKPQ n e k of
        [x] -> [(x,x)]
        [x,y] -> [(x,y),(y,x)]

    let p1 = [1]
        q1 = [1]
        d1 = [1]
        dp1 = [1]
        dq1 = [1]
    
    let slice 1 = [(p1,q1,d1,dp1,dq1)]
        slice i = do
            let m = 2^i
            (p',q',d',dp',dq') <- slice (i-1)

            (p'', pv) <- p' >>< p
            (q'', qv) <- q' >>< q

            guard $ (pv * qv - n) `mod` (2^i) == 0

            (d'',dv) <- d' >>< d
            guard $ (e * dv - (k*(n-pv-qv+1) + 1)) `mod` m == 0

            (dp'',dpv) <- dp' >>< dp
            guard $ (e * dpv - (kp*(pv-1) + 1)) `mod` m  == 0

            (dq'',dqv) <- dq' >>< dq
            guard $ (e * dqv - (kq*(qv-1) + 1)) `mod` m == 0

            return (p'',q'',d'',dp'',dq'')

    (p,q,_,_,_) <- slice (length (fromPartial p))
    return (fromDigits 2  p, fromDigits 2 q)


main = breakKey n e target
