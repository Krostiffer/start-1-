module AB2_4 where

import Prelude ()
import SPLib.Basic (Ord, Int, trace, undefined, if', (==), (<), (-)) 
import SPLib.List (sort, length, remove, first, removeFirst, isEmpty, last, prepend, append, concat, map, filter, foldl)


fromToList :: Int -> Int -> [Int]
fromToList i j = [i .. j]

removeDuplicatesFromSortedList :: (Ord a) => [a] -> [a] 
removeDuplicatesFromSortedList [] = [] 
removeDuplicatesFromSortedList (x:lr) = foldl f [x] lr 
  where
    f acc x = if' ((last acc) < x) (append x acc) acc

subsets' :: (Ord a) => [a] -> [[a]] 
subsets' x = removeDuplicatesFromSortedList (sort (subsets x))     
     
subsets  :: (Ord a) => [a] -> [[a]] --duplicates allowed 
kSubsets :: (Ord a) => Int -> [a] -> [[a]] --all subsets with k elements without duplicates

-- *** DO NOT MODIFY ABOVE CODE ***

subsets = undefined
kSubsets = undefined
