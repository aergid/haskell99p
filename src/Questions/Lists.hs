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
myLast' = foldr1 (\x acc -> acc)

myLast'' ::  [a] -> a
myLast'' = foldl1 (\acc x -> x)

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
myButLast (x:[]) = error "List too short"
myButLast (x:y:[]) = x
myButLast (x:y:zs) = myButLast $ y:zs

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
