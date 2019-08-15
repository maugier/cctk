{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module CCTK.Partial where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid

newtype Partial a = Partial { fromPartial :: [Maybe a] }
    deriving (Functor, Semigroup, Monoid)

instance Show a => Show (Partial a) where
    show (Partial c) = concatMap show' (reverse c) where
        show' Nothing = "?"
        show' (Just x) = show x

lsb n (Partial xs) = Partial (take n xs)
msb n (Partial xs) = Partial (replicate n Nothing ++ drop n xs)

match :: Eq a => Maybe a -> Maybe a -> Bool
match (Just x) (Just y) = x == y
match _  _              = True

matches :: Eq a => Partial a -> Partial a -> Bool
matches (Partial a) (Partial b) = and $ zipWith match a b

fromKnown :: [a] -> Partial a
fromKnown = Partial . map Just 

known :: Partial a -> Bool
known = not . any isNothing . fromPartial

unknown = Partial [Nothing]

unknowns = Partial . flip replicate Nothing

exhaust :: Enum a => (a,a) -> Partial a -> [[a]]
exhaust (a,b) (Partial xs) = sequence [ (case x of
    Nothing -> enumFromTo a b
    Just x -> [x]) | x <- xs]


extend :: Partial a -> Partial a
extend (Partial xs) = Partial (xs ++ [Nothing])


combineOne :: Eq a => Maybe a -> Maybe a -> Maybe (Maybe a)
combineOne a Nothing = Just a
combineOne Nothing b = Just b
combineOne (Just a) (Just b) | a == b    = Just (Just a)
                             | otherwise = Nothing

combine :: Eq a => Partial a -> Partial a -> Maybe (Partial a)
combine (Partial []) b = Just (Partial [])
combine a (Partial []) = Just a
combine (Partial (a:as)) (Partial (b:bs)) = do
    t <- combineOne a b 
    Partial ts <- combine (Partial as) (Partial bs)
    return $ Partial (t:ts)

(><) :: (Eq a, MonadPlus m) => Partial a -> Partial a -> m (Partial a)
a >< b = case combine a b of
    Nothing -> mzero
    Just a -> return a


(>?<) :: Eq a => Partial a -> Partial a -> Bool
(>?<) = matches

zipWithPartial :: (a -> b -> c) -> Partial a -> Partial b -> Partial c
zipWithPartial f (Partial as) (Partial bs) = Partial (zipWith (liftM2 f) as bs)
