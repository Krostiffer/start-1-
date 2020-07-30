module AB3_1 where

import Prelude ()
import AB2_1 (swap, unorderedWitnesses)
import SPLib.Basic (Ord, Int, Bool, trace, undefined, if', id, not, fst, max, (.), (<), (+), (-))
import SPLib.List (first, isEmpty, length, get, foldl)


type Operation a = (a -> a)
type Predicate a = (a -> Bool)

if2     :: (Predicate a) -> (Operation a) -> (Operation a)
while   :: (Predicate a) -> (Operation a) -> (Operation a) 
repeat  :: Int -> (Operation a) -> (Operation a)
foreach :: [b] -> (Operation (a,b)) -> (Operation a) 

tailRecursion :: (Operation a) -> (Operation a) -> (Predicate a) -> (Operation a)

primitiveSort :: (Ord a) => [a] -> [a]

bubbleSort :: (Ord a) => (Operation [a])
swapPred   :: (Ord a) => (Predicate ([a],Int))
swapOp     :: (Ord a) => (Operation ([a],Int))

-- *** DO NOT MODIFY ABOVE CODE ***

if2 pred op state = if' (pred (state)) (op state) state
while pred op state = if' (pred (state)) (while pred op (op state)) state
repeat i op state = if' (0 < i) (repeat (i-1) op (op state)) state

foreach lst op state = foldl f state lst
  where
    f state' el =   fst (op (state', el))


tailRecursion g h p state = (g.while p h) state

swapHelp lst = swap ((first.unorderedWitnesses) lst) (((first.unorderedWitnesses) lst) +1) lst

primitiveSort lst = (while (not.isEmpty.unorderedWitnesses) (swapHelp)) lst

bubbleSort lst = program lst
  where
    n = length lst
    program = 
        (repeat (n-1)
          (foreach [0 .. n-2] (if2 swapPred swapOp)
          )
        )
        
swapPred (lst,i) = (get (i+1) lst) < (get i lst)
swapOp (lst,i) = ((swap i (i+1) lst), undefined)
