module Likelihood (Histogram, histogram, histFreq, score, histFromFile, defaultBytes,
                   best, bests, normalize, entropy, (|$|), (|*|)) where

import Data.Char
import qualified Data.Map as M
import Data.Map (Map, fromList, fromListWith, findWithDefault, mapKeysWith, toList)
import qualified Data.ByteString.Lazy as B
import Data.List (sortBy , maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)

type Histogram k = Map k Double


histogram :: Ord k => [k] -> Histogram k
histogram = fromListWith (+) . map (\k -> (k,1))

histFreq :: Ord k => [(k,Double)] -> Histogram k
histFreq = normalize . fromList

normalize n = M.map (/ maximum (M.elems n)) n 

normalize' n = M.map (/ sum (M.elems n)) n

score :: Ord k => Histogram k -> [k] -> Double
score m = sum . map (negate . log  . flip (findWithDefault 1) m)

bests :: Histogram a -> [(a,Double)]
bests = reverse . sortBy (comparing snd) . toList

best :: Histogram a -> a
best = fst . maximumBy (comparing snd) . toList

histFromFile f = B.readFile f >>= return . histogram . B.unpack

defaultBytes :: Histogram Word8
defaultBytes = normalize . fromListWith (+) $ byteCount ++ [(x,1) | x <- [0..255]]

textBytes = M.mapWithKey (\k a -> if chr (fromIntegral k) `elem` " .,!?+-" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'0'] then a else 0.000001) defaultBytes

byteCount = [(9,144),(10,172065),(12,10),(13,6359),(32,903319),(33,431),(34,1358),(35,460),(36,75),(37,511),(38,9356),(39,6813),(40,13377),(41,13489),(42,646),(43,4551),(44,12776),(45,9806),(46,47367),(47,179551),(48,138265),(49,30661),(50,55552),(51,48834),(52,28428),(53,45934),(54,24841),(55,26279),(56,38631),(57,25568),(58,48597),(59,9702),(60,227703),(61,2944),(62,227708),(63,1713),(64,1541),(65,7344),(66,27714),(67,22095),(68,20877),(69,21439),(70,6026),(71,3751),(72,3768),(73,11286),(74,776),(75,523),(76,9283),(77,4458),(78,13869),(79,7889),(80,10272),(81,239),(82,30229),(83,13744),(84,22801),(85,29150),(86,784),(87,2025),(88,459),(89,6076),(90,137),(91,5841),(92,53),(93,5841),(94,16),(95,282275),(96,36),(97,308952),(98,82152),(99,185842),(100,163847),(101,561599),(102,183557),(103,113944),(104,62272),(105,340221),(106,2968),(107,37168),(108,277212),(109,112984),(110,259352),(111,393856),(112,143240),(113,13026),(114,362535),(115,263072),(116,422578),(117,185756),(118,28394),(119,51245),(120,15040),(121,40527),(122,5384),(124,4),(126,8),(128,253),(129,154),(130,133),(131,415),(132,106),(133,44),(134,34),(135,70),(136,173),(137,108),(138,113),(139,104),(140,60),(141,87),(142,102),(143,25),(144,178),(145,131),(146,27),(147,28),(148,293),(149,335),(150,72),(151,47),(152,57),(153,85),(154,61),(155,65),(156,44),(157,73),(158,51),(159,18),(160,605),(161,63),(162,66),(163,37),(164,23),(165,52),(166,30),(167,22),(168,32),(169,19),(170,46),(171,41),(172,11),(173,52),(174,10),(175,5),(176,72),(177,130),(178,74),(179,91),(180,40),(181,99),(182,34),(183,33),(184,395),(185,184),(186,47),(187,50),(188,85),(189,246),(190,89),(191,128),(194,14),(195,19),(196,3),(197,4),(201,10),(202,6),(203,9),(204,3),(206,454),(207,249),(208,220),(209,110),(211,1),(212,1),(215,1),(224,413),(225,934),(226,1322),(227,5),(239,2)] ++ [(x,1) | x <- [0..255]]

(|$|) :: (Ord a, Ord b) => (a -> b) -> Histogram a -> Histogram b
(|$|) = (normalize .) . mapKeysWith (+)
infixl 4 |$|

(|*|) :: (Ord a, Ord b) => Histogram a -> Histogram b -> Histogram (a,b)
xs |*| ys = normalize $ fromListWith (+) [ ((x,y), px*py) | (x,px) <- toList xs, (y,py) <- toList ys ]
infixl 4 |*|

entropy :: Histogram a -> Double
entropy h = 0 - sum [ p * log p | p <- M.elems (normalize' h) ] / log 2
