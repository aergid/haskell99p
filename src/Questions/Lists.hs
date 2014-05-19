module Questions.Lists where

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

element_at ::  (Ord a, Num a, Enum a) => [t] -> a -> t
element_at [] _ = error "list is empty!"
element_at (x:_) 1 = x
element_at (_:xs) n | n > 0 = element_at xs $ pred n
                | n < 0 = error "negative index!"

element_at' ::  [c] -> Int -> c
element_at' xs n = head . drop (n - 1) $ xs

element_at'' ::  [c] -> Int -> c
element_at'' xs n = last . take n $ xs

element_at''' ::  (Num b, Eq b, Eq c, Enum b) => [c] -> b -> c
element_at''' xs n = fst . head. filter (\cs -> snd cs == n ) $ zip xs [1..]


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
 --}
