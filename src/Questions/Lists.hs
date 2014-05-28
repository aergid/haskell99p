module Questions.Lists where

import           Control.Applicative
import           Data.Char           (ord, chr)
import           Data.List           (group)

{-
--    Problem 1
--    (*) Find the last element of a list.
--    (Note that the Lisp transcription of this problem is incorrect.)
--    Example in Haskell:
--    Prelude> myLast [1,2,3,4]
--    4
--    Prelude> myLast ['x','y','z']
--    'z'
-}

myLast ::  [t] -> t
myLast (x:[]) = x
myLast (_:xs) = myLast xs
myLast [] = error "Empty list has no last element"

myLast' ::  [a] -> a
myLast' = foldr1 (\_ acc -> acc)

myLast'' ::  [a] -> a
myLast'' = foldl1 (\_ x -> x)

myLast''' ::  [a] -> a
myLast''' = foldl1 (const id)

--
{-
 --myLast' = foldr1 (const id)
 --
 --myLast'' = foldr1 (flip const)
 --
 --myLast''' = head . reverse
 --
 --myLast'''' = foldl1 (curry snd)
 --
 --myLast''''' [] = error "No end for empty lists!"
 -}


{-
--  2 Problem 2
--
--(*) Find the last but one element of a list.
--
--(Note that the Lisp transcription of this problem is incorrect.)
--
--Example in Haskell:
--
--Prelude> myButLast [1,2,3,4]
--3
--Prelude> myButLast ['a'..'z']
--'y'V
 -}

myButLast ::  [t] -> t
myButLast ([]) = error "List too short"
myButLast (_:[]) = error "List too short"
myButLast (x:_:[]) = x
myButLast (_:y:zs) = myButLast $ y:zs

myButLast' ::  [c] -> c
myButLast' = head . tail . reverse

myButLast'' ::  [c] -> c
myButLast'' = last . init

{-
 --myButLast :: [a] -> a
 --myButLast = last . init
 --
 --myButLast' x = reverse x !! 1
 --
 --myButLast'' [x,_]  = x
 --myButLast'' (_:xs) = myButLast'' xs
 --
 --myButLast''' (x:(_:[])) = x
 --myButLast''' (_:xs) = myButLast''' xs
 --
 --myButLast'''' = head . tail . reverse
 --
 --We can also use a pair to keep the last two elements processed from the list. We take advantage of lazy evaluation, such that if the list is too small, the initial elements in the pair get evaluated, resulting in an error:
 --
 --myButLast''''' = snd.(foldl (\(a,b) c -> (c,a)) (e1, e2))
 --    where e1 = error "List too small!"
 --          e2 = error "List is null!"
 -}

{-
 --Problem 3
 --
 --(*) Find the K'th element of a list. The first element in the list is number 1.
 --
 --Example:
 --
 --* (element-at '(a b c d e) 3)
 --c
 --
 --Example in Haskell:
 --
 --Prelude> elementAt [1,2,3] 2
 --2
 --Prelude> elementAt "haskell" 5
 --'e'
 -}

elementAt ::  (Ord a, Num a, Enum a) => [t] -> a -> t
elementAt [] _ = error "list is empty!"
elementAt (x:_) 1 = x
elementAt (_:xs) n | n > 0 = elementAt xs $ pred n
                    | n < 0 = error "negative index!"
elementAt _ _ = error "list is empty!"

elementAt' ::  [c] -> Int -> c
elementAt' xs n = head . drop (n - 1) $ xs

elementAt'' ::  [c] -> Int -> c
elementAt'' xs n = last . take n $ xs

elementAt''' ::  (Num a, Eq a, Enum a) => [c] -> a -> c
elementAt''' xs n = fst . head. filter (\cs -> snd cs == n ) $ zip xs [1..]


{-
 --This is (almost) the infix operator !! in Prelude, which is defined as:
 --
 --(!!)                :: [a] -> Int -> a
 --(x:_)  !! 0         =  x
 --(_:xs) !! n         =  xs !! (n-1)
 --
 --Except this doesn't quite work, because !! is zero-indexed, and element-at should be one-indexed. So:
 --
 --elementAt :: [a] -> Int -> a
 --elementAt list i    = list !! (i-1)
 --
 --Or without using the infix operator:
 --
 --elementAt' :: [a] -> Int -> a
 --elementAt' (x:_) 1  = x
 --elementAt' [] _     = error "Index out of bounds"
 --elementAt' (_:xs) k
 --  | k < 1           = error "Index out of bounds"
 --  | otherwise       = elementAt' xs (k - 1)
 --
 --Alternative version:
 --
 --elementAt'' :: [a] -> Int -> a
 --elementAt'' (x:_) 1  = x
 --elementAt'' (_:xs) i = elementAt'' xs (i - 1)
 --elementAt'' _ _      = error "Index out of bounds"
 --
 --This does not work correctly on invalid indexes and infinite lists, e.g.:
 --
 --elementAt'' [1..] 0
 --
 --A few more solutions using prelude functions:
 --
 --elementAt'' xs n | length xs < n = error "Index out of bounds"
 --           | otherwise = fst . last $ zip xs [1..n]
 --
 --elementAt''' xs n = head $ foldr ($) xs
 --                         $ replicate (n - 1) tail
 ---- Negative indeces not handled correctly:
 ---- Main> elementAt''' "haskell" (-1)
 ---- 'h'
 --
 --elementAt_w' xs n = last . take n $ xs -- wrong
 ---- Main> map (elementAt_w' [1..4]) [1..10]
 ---- [1,2,3,4,4,4,4,4,4,4]
 --
 --elementAt_w'' xs n = head . reverse . take n $ xs -- wrong
 ---- Main> map (elementAt_w'' [1..4]) [1..10]
 ---- [1,2,3,4,4,4,4,4,4,4]
 --
 --elementAt_w''' xs n = head . drop (n - 1) $ xs -- wrong
 ---- Main> map (elementAt_w''' [1..4]) [0..10]
 ---- [1,1,2,3,4,*** Exception: Prelude.head: empty list
 --
 --or
 --elementAt_w'
 --correctly in point-free style:
 --
 --elementAt_w'pf = (last .) . take . (+ 1)
 --
 --Pedantic note: the above definition of
 --elementAt_w'pf
 --does not conform to the order of arguments specified by the question, but the following does:
 --
 --elementAt_w'pf' = flip $ (last .) . take . (+ 1)
 -}
{-
 --Problem 4
 --
 --(*) Find the number of elements of a list.
 --
 --Example in Haskell:
 --
 --Prelude> myLength [123, 456, 789]
 --3
 --Prelude> myLength "Hello, world!"
 --13
 -}

myLength :: [a] -> Int
myLength = fst . last . zip [1..]

myLength' :: [a] -> Int
myLength' = foldl (\acc _ -> succ acc) 0

myLength'' :: [a] -> Int
myLength'' [] = 0
myLength'' (_:xs) = 1 + myLength xs

myLength''' :: [a] -> Int
myLength''' = linLength 0
        where linLength acc [] = acc
              linLength acc (_:xs) = linLength (succ acc) xs

{-
 -We can also change each element into our list into a "1" and then add them all together.
 -
 -myLength :: [a] -> Int
 -myLength = sum . map (\_->1)
 -myLength3 =  foldr (\_ -> (+1)) 0
 -myLength4 =  foldr ((+) . (const 1)) 0
 -myLength5 =  foldr (const (+1)) 0
 -myLength6 =  foldl (const . (+1)) 0V
-}

{-
 -Problem 5
 -
 -(*) Reverse a list.
 -
 -Example in Haskell:
 -
 -Prelude> myReverse "A man, a plan, a canal, panama!"
 -"!amanap ,lanac a ,nalp a ,nam A"
 -Prelude> myReverse [1,2,3,4]
 -[4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


{-
 -Problem 6
 -
 -(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
 -
 -Example in Haskell:
 -
 -*Main> isPalindrome [1,2,3]
 -False
 -*Main> isPalindrome "madamimadam"
 -True
 -*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
 -True
-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

{-
 -With monomorphism restriction:
 -isPalindrome1 xs = (uncurry (==) . (id &&& reverse)) xs
 -
 -Point free with no monomorphism restriction:
 -isPalindrome1 = (uncurry (==) . (id &&& reverse))
 -
 -isPalindrome''' :: (Eq a) => [a] -> Bool
 -isPalindrome''' = Control.Monad.liftM2 (==) id reverse
 -Or even:
 -isPalindrome'''' :: (Eq a) => [a] -> Bool
 -isPalindrome'''' = (==) Control.Applicative.<*> reverse
-}
{-
 -
 - 7 Problem 7
 -
 -(**) Flatten a nested list structure
 - data NestedList a = Elem a | List [NestedList a]
 -
 -*Main> flatten (Elem 5)
 -[5]
 -*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
 -[1,2,3,4,5]
 -*Main> flatten (List [])
 -[]
-}

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = foldl (\acc a -> acc ++ flatten a) (flatten x) xs

flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List xs) = concatMap flatten' xs

{- Problem 8
 -(compress '(a a a a b c c a a d e e e e))
 -(A B C A D E)
-}

myCompress :: (Eq a) => [a] -> [a]
myCompress = map head . group

myCompress' :: Eq a => [a] -> [a]
myCompress' [] = []
myCompress' (x:xs) = let (_, others) = span (== x) xs in
                        x: myCompress' others
    {- :P
     -compress []     = []
     -compress (x:xs) = x : (compress $ dropWhile (== x) xs)
     -}

{- Problem 9
 -
 - *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
 'a', 'd', 'e', 'e', 'e', 'e']
 ["aaaa","b","cc","aa","d","eeee"]
-}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs@(x:_) = let (same, others) = span (== x) xs
                    in same : pack others


{-Problem 10
 -encode "aaaabccaadeeee"
 -[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

myEncode :: String -> [(Int, Char)]
myEncode xs = let mix = [length, ord . head] <*> pack xs
                  mid = length mix `div` 2
                  (lengths, leaders) = splitAt mid mix
                in zip lengths $ map chr leaders

myEncode' :: String -> [(Int, Char)]
myEncode' xs = let partitioned = pack xs
                   lengths = map length partitioned
                   leaders = map head partitioned
                in zip lengths leaders
-- "Use group, Luke"

encode xs = map (\x -> (length x,head x)) (group xs)
{-
 -
 -which can also be expressed as a list comprehension:
 -
 -[(length x, head x) | x <- group xs]
 -
 -Or writing it Pointfree (Note that the type signature is essential here to avoid hitting the Monomorphism Restriction):
 -
 -encode :: Eq a => [a] -> [(Int, a)]
 -encode = map (\x -> (length x, head x)) . group
 -
 -Or (ab)using the "&&&" arrow operator for tuples:
 -
 -encode :: Eq a => [a] -> [(Int, a)]
 -encode xs = map (length &&& head) $ group xs
 -
 -Or using the slightly more verbose (w.r.t.
 -(&&&)
 -) Applicative combinators:
 -
 -encode :: Eq a => [a] -> [(Int, a)]
 -encode = map ((,) <$> length <*> head) . pack
 -
 -Or with the help of foldr (pack is the resulting function from P09):
 -
 -encode xs = (enc . pack) xs
 -    where enc = foldr (\x acc -> (length x, head x) : acc) []
 -
 -Or using takeWhile and dropWhile:
 -
 -encode [] = []
 -encode (x:xs) = (length $ x : takeWhile (==x) xs, x)
 -                 : encode (dropWhile (==x) xs)
 -
 -Or without higher order functions:
 -
 -encode []     = []
 -encode (x:xs) = encode' 1 x xs where
 -    encode' n x [] = [(n, x)]
 -    encode' n x (y:ys)
 -        | x == y    = encode' (n + 1) x ys
 -        | otherwise = (n, x) : encode' 1 y ys
 -
 -Or we can make use of zip and group:
 -
 - 
 -import List
 -encode :: Eq a => [a] -> [(Int, a)]
 -encode xs=zip (map length l) h where 
 -    l = (group xs)
 -    h = map head l
 -}
