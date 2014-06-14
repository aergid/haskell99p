module Questions.Arithmetic where

{-
 --Problem 31
 --
 --(**) Determine whether a given integer number is prime.
 --
 --Example in Haskell:
 --
 --P31> isPrime 7
 --True
-}
--first version
--primes :: [Integer]
--primes = map head $ iterate sieve [2..]

--sieve :: [Integer] -> [Integer]
--sieve (x:xs) = filter ((/=0) . (`mod` x)) xs

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x: sieve (filter ((/=0) . (`mod` x)) xs)

isPrime :: Integer -> Bool
isPrime x = all ((/=0) . (x `mod`)) $ takeWhile (<= candidateRoot) primes
    where candidateRoot = floor . sqrt $ fromIntegral x

{-
 -isPrime' n | n < 4 = n /= 1 
 -isPrime' n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
 -        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
 -              m = floor . sqrt $ fromIntegral n
 -}
