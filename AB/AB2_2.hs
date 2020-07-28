module AB2_2 where

import Prelude (pred, succ)
import SPLib.Basic (Int, trace, undefined, if', id, (==))


add  :: Int -> Int -> Int
mult :: Int -> Int -> Int
pow  :: Int -> Int -> Int
genericArithmetic :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int

-- *** DO NOT MODIFY ABOVE CODE ***

-- as long as y is not null add 1 to the result and sub 1 from y
add x y = if' (y == 0) (x) (add (succ x) (pred y))

-- we need a third value storing the value that is to be added in the recursion
multHelp :: Int -> Int -> Int -> Int
multHelp x y a = if' ((pred y) == 0) (x) (multHelp (add a x) (pred y) a) 

-- precheck  if x or y are 0 as addition cannot handle this correctly (only if both are 0)
mult x y = if' (x == 0) 0 (if' (y == 0) 0 (multHelp x y x))

powHelp :: Int -> Int -> Int -> Int
powHelp x y m = if' ((pred y) == 0) (x) (powHelp (mult x m) (pred y) m) 

-- precheck if y == 0 (x to the power of 0) to result in 1 as the multiplication can only handle if x == 0
pow x y =  if' (y == 0) 1 (powHelp x y x)

genericArithmetic = undefined