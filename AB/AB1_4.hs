module AB1_4 where

import Prelude ()
import AB1_2 (infiniteList)
import SPLib.Basic (Int, trace, undefined, (<), (+), (*), (^), mod, div)
import SPLib.List (take, reverse, zip, find, map, foldl)


type Base = Int -- at least 2

toDigits   :: Base -> Int -> [Int] -- 2 4 -> [1,0,0] ; 10 123405 -> [1,2,3,4,0,5]
fromDigits :: Base -> [Int] -> Int -- 2 [1,0,0] -> 4 ; 10 [1,2,3,4,0,5] -> 123405
length     :: Base -> Int -> Int -- 2 4 -> 3 ; 10 123405 -> 6 ; 7 0 -> 1

-- *** DO NOT MODIFY ABOVE CODE ***

digit :: Base -> Int -> Int -> Int
digit b n i = mod (div n (b ^ i)) b

toDigits b n = reverse (map (digit b n) (take (length b n) (infiniteList 0)))

zipList :: [Int] -> [(Int, Int)]
zipList lst = zip lst (infiniteList 0)

mapList :: [(Int, Int)] -> Int -> [Int]
mapList ((a, b) : lst) base = (a * base ^ b) : mapList lst base
mapList _ _ = []

foldList :: [Int] -> Int
foldList lst = foldl (+) 0 lst
fromDigits base lst = foldl (+) 0 (mapList (zipList (reverse (lst))) base)

length b z = find (f b) (infiniteList 1)
  where
    f b x =  z < (b ^ x) --x ist element der infiniteList; (b^x)-1 ist die größte Zahl, die mit b stellen in basis b dargestellt werden kann, also z<=(b^x)-1 umgeformt da in Int z < (b^x)