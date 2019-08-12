{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alphabetical where

import Control.Monad (mapM)
import Data.Char

newtype Letter = Letter { intFromLetter :: Int }
    deriving (Eq,Ord,Enum)

type Letters = [Letter]

letterFromInt x = Letter (x `mod` 26)

letterFromChar :: Char -> Either Char Letter
letterFromChar x | ord x >= ord 'A' && ord x <= ord 'Z' = Right (Letter (ord x - ord 'A'))
                 | ord x >= ord 'a' && ord x <= ord 'z' = letterFromChar (toUpper x)
                 | otherwise = Left x

letter k = let Right x = letterFromChar k in x
letters = map letterFromChar

unletter (Left c) = c
unletter (Right (Letter k)) = chr (ord 'A' + k)

unletters = map unletter

charFromLetter :: Letter -> Char
charFromLetter = chr . (ord 'A' +) . intFromLetter

onLetter f = letterFromInt . f . intFromLetter
onLetter2 f x y = letterFromInt (f (intFromLetter x) (intFromLetter y)) 

display :: [Maybe Letter] -> String
display = map $ \c -> case c of { Nothing -> '?' ; Just l -> charFromLetter l }

instance Show Letter where
    show     = show . charFromLetter
    showList = showList . map charFromLetter

instance Num Letter where
    fromInteger = letterFromInt . fromInteger 
    (+) = onLetter2 (+)
    (-) = onLetter2 (-)
    (*) = onLetter2 (*)
    negate = onLetter negate
    abs    = id
    signum _ = Letter 1

instance Read Letter where
    readsPrec _ (c:cs) = case letterFromChar c of
        Right l -> [(l, cs)]
        _       -> []
