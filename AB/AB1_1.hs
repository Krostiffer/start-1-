module AB1_1 where

import Prelude ()
import SPLib.Basic (Int, Bool(True,False), trace, undefined, if', (==), (&&), (||), (<), (+), (-))
import SPLib.List (first, isEmpty, prepend, reduce, foldl, foldr)


minimum  :: [Int] -> Int
maximum  :: [Int] -> Int

bigAnd   :: [Bool] -> Bool -- true iff all elements are true
bigOr    :: [Bool] -> Bool -- true iff at least one element is true
bigXor   :: [Bool] -> Bool -- true iff an odd number of elements is true

sum :: [Int] -> Int -- sum of all elements in the list
alternatingSum  :: [Int] -> Int -- [1,2,3,4,5,6] -> 1 - 2 + 3 - 4 + 5 - 6 ...
alternatingSum2 :: [Int] -> Int -- [1,2,3,4,5,6] -> 1 + 2 - 3 + 4 - 5 + 6 ...

bigAnd2 :: [Bool] -> Bool --same as bigAnd and for the empty list it should return True

-- *** DO NOT MODIFY ABOVE CODE ***

minimum (lst) = reduce (\x y -> if' (x < y) x y) lst

maximum (lst) = reduce (\x y -> if' (x < y) y x) lst

bigAnd (lst) = reduce (&&) lst 
bigOr (lst) = reduce (||) lst 
bigXor (lst) = reduce (\x y -> if' (x || y == True) (if' (x && y == True) False True) False) lst 

sum (lst) = reduce (+) lst
alternatingSum (lst) = foldr (-) 0 lst
alternatingSum2(lst) = foldr (-) 0 (prepend (first(lst) + first(lst)) lst)

bigAnd2(lst) = if' (isEmpty(lst)) True (reduce (&&) lst)
