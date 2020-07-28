module AB1_3 where

import Prelude ()
import AB1_1 (bigAnd2)
import AB1_2 (infiniteList,listFromTo)
import SPLib.Basic (Int, trace, undefined, (/=), (+), (-), (*), mod)
import SPLib.List (first, removeFirst, concat, append, take, map, filter, foldl)


crossList :: [a] -> [b] -> [(a,b)] -- [1,2] [3,4] -> [(1,3),(1,4),(2,3),(2,4)]
genCrossList :: [[a]] -> [[a]] -- [[1,2],[3,4],[5,6]] -> [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],...,[2,4,6]]
primes :: Int -> [Int] -- 5 -> [2,3,5,7,11]

-- *** DO NOT MODIFY ABOVE CODE ***

makeTuple :: a -> b -> (a, b)
makeTuple a b = (a, b)

mapHelp :: [b] -> a -> [(a, b)]
mapHelp lst2 a = map (makeTuple a) lst2

crossList lst1 lst2 = concat (map (mapHelp (lst2)) lst1)

makeTuple2 :: [a] -> a -> ([a], a)
makeTuple2 acc_el lst_el = (acc_el, lst_el)

mapLst :: [a] -> [a] -> [([a], a)]
mapLst lst acc_el = map (makeTuple2 acc_el) lst

mapAcc :: [[a]] -> [a] -> [([a], a)]
mapAcc acc lst = concat (map (mapLst (lst)) acc)

genCrossList lst = foldl crossList2 acc lst'
  where
    crossList2 x y = map g (mapAcc x y)
      where
        g (lst, el) = append el lst
    lst' = removeFirst (lst)
    acc = map f (first lst)
      where
        f el = [el]


primes x = take x allPrimes

filterFun a = bigAnd2 (map (/= 0) (map (mod a) (listFromTo 2 (a -1))))

allPrimes :: [Int]
allPrimes = filter (filterFun) (infiniteList 2)

