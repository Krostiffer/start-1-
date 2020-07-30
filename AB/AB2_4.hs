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

getIndices a = fromToList 0 (length a - 1)

-- Returns a list of the direct sublists of a list [1,2,3] -> [[2,3],[1,3],[1,2]]
getList' :: [a] -> [[a]]
getList' lst = map (\a -> remove a lst) (getIndices lst)

-- moves over every direct sublist and calls itself again with the sublists. Adds the list it's called with and every list of the length 1 to the output list.
subsetsHelp :: [a] -> [[a]] -> [[a]]
subsetsHelp lst a = foldl f (append lst a) (getList' lst)
  where
    f acc el = if' ((length el) == 1) (append el acc) (subsetsHelp el acc)

-- calls the recursive subsetsHelp, checks wether the list is empty beforehand and filters the output to remove empty subsets
subsets lst = if' (isEmpty lst) [] (filter (f) (subsetsHelp lst []))
  where
    f x = 0 < (length x)

-- Filters the subsets so that only the subsets with expected length are returned
kSubsets i a = filter f (subsets' a)
  where 
    f x = i == (length x)