module Util where

chunk n = map (take n) . takeWhile (not.null) . iterate (drop n)

