module AB2_2 where

import Prelude (pred, succ)
import SPLib.Basic (Int, trace, undefined, if', id, (==))


add  :: Int -> Int -> Int
mult :: Int -> Int -> Int
pow  :: Int -> Int -> Int
genericArithmetic :: (Int -> Int) -> (Int -> Int -> Int) -> Int -> Int -> Int

-- *** DO NOT MODIFY ABOVE CODE ***

add x y = genericArithmetic id addhelp x y
addhelp x y = add (succ x) (pred y) --a + b = (a+1)+(b-1)

mult x y = genericArithmetic pred multhelp x y --pred für case y = 1 -> return x
multhelp x y = if' (y == 0) 0 (add (x) (mult x (pred y))) -- a * b = a+(a*(b-1)) case y=0 -> return 0

pow x y = genericArithmetic pred powhelp x y --pred für case y = 1 -> return x
powhelp x y = if' (y == 0) 1 (mult (x) (pow x (pred y))) -- a ^ b = a*(a*(b-1)) case y=0 -> return 1
genericArithmetic f a x y = if' ((f y) == 0) (x) (a x y)