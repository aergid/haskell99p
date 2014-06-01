module Questions.Lists2 where

import           Control.Applicative
import           Control.Monad       (liftM)
import           System.Random
{--
 --P21> insertAt 'X' "abcd" 2
 --"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt _ [] _ = error "can't insert item"
insertAt x (y:ys) n | n > 0 = y : insertAt x ys (n - 1)
                | otherwise = error "can't insert item"

{-
 --Problem 22
 --
 --Create a list containing all integers within a given range.
 --
 --Example in Haskell:
 --
 --Prelude> range 4 9
 --[4,5,6,7,8,9]
-}

myRange :: (Integral a) => a -> a -> [a]
myRange a b = [a..b]

myRange' :: (Integral a) => a -> a -> [a]
myRange' = enumFromTo

{-
 --Problem 23
 --
 --Extract a given number of randomly selected elements from a list.
 --
 --Example in Haskell:
 --
 --Prelude System.Random>rndSelect "abcdefgh" 3 >>= putStrLn
 --eda
-}

rndSelect :: [a] -> Int -> IO [a]
--rndSelect xs n = getStdGen >>= return . take n . pickFromList xs
rndSelect xs n = liftM (take n . pickFromList xs) getStdGen

pickFromList :: (RandomGen g) => [a] -> g -> [a]
pickFromList xs g = (xs !!) <$> randomRs (0, length xs -1) g
