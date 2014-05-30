module Quest.Lists2 where

import           Control.Applicative
import           Data.List           (group)
{-
 --Problem 11
 --
 --(*) Modified run-length encoding.
 --Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
 --Example:
 --P11> encodeModified "aaaabccaadeeee"
 --[Multiple 4 'a',Single 'b',Multiple 2 'c',
 -- Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Symbol a = Single a |
                Multiple Int a deriving Show

encodeModified :: (Eq a) => [a] -> [Symbol a]
encodeModified = map (\x -> if length x == 1 then Single $ head x else Multiple (length x) (head x)) . group

{-
 --Problem 12
 --
 --(**) Decode a run-length encoded list.
 --
 --Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
 --
 --Example in Haskell:
 --
 --P12> decodeModified
 --       [Multiple 4 'a',Single 'b',Multiple 2 'c',
 --        Multiple 2 'a',Single 'd',Multiple 4 'e']
 --"aaaabccaadeeee"
 -}

decodeModified :: [Symbol a] -> [a]
decodeModified xs = xs >>= decode
                        where decode (Single c) = [c]
                              decode (Multiple n c) = replicate n c

-- uncurry replicate is also nice
--

{-
 --Problem 13
 --
 --(**) Run-length encoding of a list (direct solution).
 --
 --Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
 --
 --Example:
 --
 --* (encode-direct '(a a a a b c c a a d e e e e))
 --((4 A) B (2 C) (2 A) D (4 E))
 --
 --Example in Haskell:
 --
 --P13> encodeDirect "aaaabccaadeeee"
 --[Multiple 4 'a',Single 'b',Multiple 2 'c',
 -- Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

encodeDirect :: (Eq a) => [a] -> [Symbol a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' :: Eq a => Int -> a -> [a] -> [Symbol a]
encodeDirect' n y [] = [encodeElement n y]
encodeDirect' n y (x:xs) | y == x    = encodeDirect' (n+1) y xs
                         | otherwise = encodeElement n y : encodeDirect' 1 x xs

encodeElement :: Int -> a -> Symbol a
encodeElement 1 y = Single y
encodeElement n y = Multiple n y

{-
 --Problem 14
 --
 --(*) Duplicate the elements of a list.
 --
 --Example in Haskell:
 --
 -- dupli [1, 2, 3]
 --[1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli xs = xs >>= replicate 2

dupli' :: [a] -> [a]
dupli' xs = xs >>= (:) <*> flip (:) []

dupli'' :: [a] -> [a]
dupli'' = foldr ((.) <$> (:) <*> (:)) []

{-
 --dupli [] = []
 --dupli (x:xs) = x:x:dupli xs
 --
 --or, using list comprehension syntax:
 --
 --dupli list = concat [[x,x] | x <- list]
 -}


{-
 -Problem 15
 --
 --(**) Replicate the elements of a list a given number of times.
 --
 --Example:
 --
 --* (repli '(a b c) 3)
 --(A A A B B B C C C)
 --
 --Example in Haskell:
 --
 --> repli "abc" 3
 --"aaabbbccc"
-}

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = foldr (const (x:)) (repli xs n) [1..n]

{-
 -- 6 Problem 16
 --
 --(**) Drop every N'th element from a list.
 --
 --*Main> dropEvery "abcdefghik" 3
 --"abdeghk"
-}

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(_,i) -> i `mod` n /= 0) $ zip xs [1..]
-- dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])
{-
 -A similar iterative solution but using a closure:
 -
 -dropEvery :: [a] -> Int -> [a]
 -dropEvery xs n = helper xs n
 -    where helper [] _ = []
 -          helper (x:xs) 1 = helper xs n
 -          helper (x:xs) k = x : helper xs (k-1)
 -
 -Yet another iterative solution which divides lists using Prelude:
 -
 -dropEvery :: [a] -> Int -> [a]
 -dropEvery [] _ = []
 -}

{-
 --Problem 17
 --
 --(*) Split a list into two parts; the length of the first part is given.
 --
 --Do not use any predefined predicates.
 --Example:
 --
 --  *Main> split "abcdefghik" 3
 --  ("abc", "defghik")
-}

mySplit :: [a] -> Int -> ([a], [a])
mySplit (x:xs) n | n > 0 = let (f,l) = mySplit xs (n-1) in (x : f, l)
mySplit xs _ = ([], xs)


{-
 --Problem 18
 --
 --(**) Extract a slice from a list.
 --
 --Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
 --
 --Example:
 --*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
 --"cdefg"
-}

slice :: [a] -> Int -> Int -> [a]
slice xs from to = [s | (s,i) <- zip xs [1..to], i >= from]

slice' :: [a] -> Int -> Int -> [a]
slice' xs i j = drop (i - 1) $ take j xs

{-
 -- Problem 19
 --
 --(**) Rotate a list N places to the left.
 --
 --Hint: Use the predefined functions length and (++).
 --
 --Examples:
 --
 --* (rotate '(a b c d e f g h) 3)
 --(D E F G H A B C)
 --
 --* (rotate '(a b c d e f g h) -2)
 --(G H A B C D E F)
 --
 --Examples in Haskell:
 --
 --*Main> rotate ['a','b','c','d','e','f','g','h'] 3
 --"defghabc"
 -- 
 --*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
 --"ghabcdef"
-}
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs    -- optimization
rotate xs n = reverse $ reverse l ++ reverse r
                where (l,r) = splitAt (n `mod` length xs) xs
