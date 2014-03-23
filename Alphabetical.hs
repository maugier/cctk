module Alphabetical where

import Control.Monad (mapM)
import Data.Char

newtype Letter = Letter { intFromLetter :: Int }

letterFromInt x = Letter (x `mod` 26)

letterFromChar :: Char -> Maybe Letter
letterFromChar x | ord x >= ord 'A' && ord x <= ord 'Z' = Just (Letter (ord x - ord 'A'))
                 | ord x >= ord 'a' && ord x <= ord 'z' = letterFromChar (toUpper x)
                 | otherwise = Nothing

letter = letterFromChar
letters = mapM letter

charFromLetter :: Letter -> Char
charFromLetter = chr . (ord 'A' +) . intFromLetter

onLetter f = letterFromInt . f . intFromLetter
onLetter2 f x y = letterFromInt (f (intFromLetter x) (intFromLetter y)) 


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
