module Group where

infixl 6 |*|
infixl 6 |/|
infixl 7 |^|

class Group g where
    (|*|) :: g -> g -> g
    i     :: g
    inv   :: g -> g
    (|^|) :: g -> Integer -> g

    _ |^| 0 = i
    g |^| 1 = g
    g |^| n | n < 0     = inv $ g |^| (0-n)
            | odd n     = g |*| (g |^| (n-1))
            | otherwise = let g2 = g |^| (n `div` 2) in g2 |*| g2

    (|/|) :: g -> g -> g
    a |/| b = a |*| inv b