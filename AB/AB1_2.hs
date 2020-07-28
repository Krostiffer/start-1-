module AB1_2 where

import Prelude ()
import SPLib.Basic (Int, trace, undefined, if', (+), (-), (==))
import SPLib.List (prepend, take)
import SPLib.Tree (Tree(Tree))


infiniteList :: Int -> [Int] -- k -> [k, k+1, k+2, k+3, ...]
listFromTo   :: Int -> Int -> [Int] -- 2 5 -> [2,3,4,5] ; listFromTo 2 1 -> []

fibTree :: (Tree Int) -- (1 1 (2 1 (3 2 (4 3 ...))))
fTree   :: (Int -> Int) -> (Tree Int) -- (1 (f 1) (2 (f 2) (3 (f 3) (4 (f 4) ...))))

-- *** DO NOT MODIFY ABOVE CODE ***
fibTreehelp :: Int -> Int -> Int -> (Tree Int)
fTreehelp :: (Int -> Int) -> Int -> (Tree Int)
infiniteList(arg1) = prepend arg1 (infiniteList(arg1 + 1))


listFromTo x y = take (y-x+1) (infiniteList x)
fibTree = fibTreehelp 1 1 1
fibTreehelp x y c = Tree c [(Tree x []),(fibTreehelp y (x+y) (c+1))]
fTree f = fTreehelp f 1
fTreehelp f c = Tree c [Tree (f c) [], (fTreehelp f (c + 1))]