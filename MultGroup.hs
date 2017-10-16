{-#LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds, FlexibleInstances, PolyKinds #-}

module MultGroup where

import GHC.TypeLits
import Data.Proxy
import Group

data M (m :: Nat) = M !Integer

instance KnownNat m => Show (M m) where
    show (M x) = show x ++ " (mod " ++ show (natVal (Proxy :: Proxy m)) ++ ")"

liftMod f (M a) (M b) = M (fromInteger (f a b))

instance KnownNat m => Num (M m) where
    fromInteger i = M (i `mod` natVal (Proxy :: Proxy m))
    (+) = liftMod (+)
    (-) = liftMod (-)
    (*) = liftMod (*)

instance KnownNat m => Group (M m) where
    i = fromInteger 1
    (|*|) = (*)
    inv (M x) = case euclid x (natVal (Proxy :: Proxy m)) of
        (a,_,1) -> fromInteger a
        _       -> error "Element not invertible"


euclid a b = go 1 0 a 0 1 b where
    go x1 y1 v1 _ _ 0 = (x1, y1, v1)
    go x1 y1 v1 x2 y2 v2 = let (q,r) = v1 `divMod` v2 in go x2 y2 v2 (x1 - q*x2) (y1 - q*y2) r

