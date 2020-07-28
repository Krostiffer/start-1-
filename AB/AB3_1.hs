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

if2 pred op state = undefined
while = undefined
repeat = undefined  
foreach = undefined

tailRecursion = undefined

primitiveSort = undefined

bubbleSort = undefined
swapPred = undefined
swapOp = undefined
