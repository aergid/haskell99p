module Questions.Lists2 where

import           Control.Applicative
import           Control.Monad       (liftM)
import           System.Random
import  Data.List (nub)
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
--rndSelect xs n = take n . pickFromList xs <$> getStdGen
rndSelect xs n = liftM (take n . pickFromList xs) getStdGen

pickFromList :: (RandomGen g) => [a] -> g -> [a]
pickFromList xs g = (xs !!) <$> randomRs (0, length xs -1) g

{-
 --Problem 24
 --
 --Lotto: Draw N different random numbers from the set 1..M.
 --
 --Example in Haskell:
 --
 --Prelude System.Random>diffSelect 6 49
 --Prelude System.Random>[23,1,17,33,21,37]
-}

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = take n . nub . randomRs (1, m) <$> getStdGen

{-
 --Problem 25
 --
 --Generate a random permutation of the elements of a list.
 --
 --Example in Haskell:
 --
 --Prelude System.Random>rnd_permu "abcdef"
 --Prelude System.Random>"badcef"
-}

rndPerm xs = diffSelect len len >>= \ind -> return $ (xs !!) . pred <$> ind
                where len = length xs

rndPerm' xs = liftM (\ind -> (xs !!) . pred <$> ind) $ diffSelect len len
                where len = length xs

--as usual overkilled
rndPerm'' xs = rndSelect xs $ length xs


{-
 -- Problem 26 Combinations
 --
 --(**) Generate the combinations of K distinct objects chosen from the N elements of a list
 --
 --In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
 --
 --Example in Haskell:
 --
 --combinations 3 "abcdef"
 --["abc","abd","abe",...]
-}

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs n | n > length xs = []
                  | n == length xs = [xs]
combinations (x:xs) n = combinationsWith x xs (n-1) ++ combinations xs n
                where combinationsWith x xs = map (x:) . combinations xs
