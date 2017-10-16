module SymbolicField where

import Data.List
import Data.Map (Map, empty, filterWithKey, fromListWith, singleton, toAscList, unionWith)

newtype Monom = Monom (Map String Int)
    deriving (Eq, Ord)
newtype Expr = Expr (Map Monom Integer)
    deriving (Eq, Ord)

showPower 1 = ""
showPower n = "^" ++ show n

showFactor (Monom m) 1 
    | m == empty  = "1"
    | otherwise   = ""
showFactor _ n = show n

filtered :: (Num a, Eq a) => Map k a -> Map k a
filtered = filterWithKey (const (/= 0))

instance Show Monom where
    show (Monom m) = concat . map (\(s,i) -> s ++ showPower i) . toAscList $ m

multiplyMonom :: Monom -> Monom -> Monom
multiplyMonom (Monom a) (Monom b) = Monom . filtered $ unionWith (+) a b

instance Show Expr where
    show (Expr e) = concat . intersperse "+" . map (\(m,i) -> showFactor m i ++ show m) . toAscList $ e

instance Num Expr where
    Expr a + Expr b = Expr . filtered $ unionWith (+) a b
    negate (Expr e) = Expr $ fmap negate e 
    Expr a * Expr b = Expr . filtered $ fromListWith (+) [ (multiplyMonom ma mb, ca*cb) | (ma,ca) <- toAscList a, (mb, cb) <- toAscList b ]
    fromInteger x = Expr (singleton (Monom empty) x)

atom :: String -> Expr
atom x = Expr (singleton (Monom (singleton x 1)) 1)

x1 = atom "x1"
y1 = atom "y1"
z1 = atom "z1"
x2 = atom "x2"
y2 = atom "y2"
z2 = atom "z2"
