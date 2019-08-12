module Util where

import qualified Data.ByteString as BS

chunk n = map (take n) . takeWhile (not.null) . iterate (drop n)

zipEither :: (b -> c -> d) -> [Either a b] -> [c] -> [Either a d]
zipEither f (Left x  : xs) ys = Left x : zipEither f xs ys
zipEither f (Right x : xs) (y:ys)  = Right (f x y) : zipEither f xs ys
zipEither _ _ _ = []


unhexlify :: String -> BS.ByteString
unhexlify = BS.pack . map (read . ("0x"++)) . chunk 2
