module CCTK.Group (
    Group(..),
    (<>),
    i,
) where

import Control.Monad (join)
import Data.Monoid

infixl 6 |/|
infixl 7 |^|

i :: Group g => g
i = mempty

class Monoid g => Group g where
    inv   :: g -> g
    (|^|) :: g -> Integer -> g

    _ |^| 0 = i
    g |^| 1 = g
    g |^| n | n < 0     = inv $ g |^| (0-n)
            | odd n     = g <> (g |^| (n-1))
            | otherwise = join (<>) (g |^| (n `quot` 2))

    (|/|) :: g -> g -> g
    a |/| b = a <> inv b

    (|\|) :: g -> g -> g
    a |\| b = inv a <> b
