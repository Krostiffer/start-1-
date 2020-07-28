module AB2_1 where

import Prelude ()
import SPLib.Basic (Ord, Int, trace, undefined, if', (<), (+), (-))
import SPLib.List (first, isEmpty, length, get, set, filter)


unorderedWitnesses :: (Ord a) => [a] -> [Int] --returns list of indices i s.t. A[i+1] < A[i]
swap :: Int -> Int -> [a] -> [a] --swaps i-th and j-th element in list
primitiveSort :: (Ord a) => [a] -> [a]

-- *** DO NOT MODIFY ABOVE CODE ***

unorderedWitnesses a = filter f [0..((length a) -2)]
    where
        f x = (get (x+1) a) < (get (x) a)

swap i j a = set (get j a) i (set (get i a) j a)

primitiveSort a = recSort a 0 (unorderedWitnesses a)
recSort a i w = if' (i < length w) goThroughWitnesses untilWitnessesEmpty
    where 
        untilWitnessesEmpty = (if' (0 < length w) (recSort a 0 (unorderedWitnesses a)) a)
        goThroughWitnesses = (recSort (swap (get i w) ((get i w) + 1) a) (i + 1) (unorderedWitnesses a))