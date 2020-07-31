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
while pred op state = if' (pred (state)) (while pred op (op state)) state --solange pred true: state ändern und rekursiv aufrufen
repeat i op state = if' (0 < i) (repeat (i-1) op (op state)) state --rekursiv operation ausführen mit runterzählendem i

foreach lst op state = foldl f state lst --foldl führt funktion auf jedes listenelement aus
  where
    f state' el =   fst (op (state', el)) --operation gibt tupel zurück, wir brauchen aber nur das erste element (state) 


tailRecursion g h p state = (g.while p h) state --führt g(h(...(x))aus, wobei h solange ausgeführt wird, wie pred true ist 
 
swapHelp lst = swap ((first.unorderedWitnesses) lst) (((first.unorderedWitnesses) lst) +1) lst --swapt ein paar von ungeordneten Elementen in lst

primitiveSort lst = (while (not.isEmpty.unorderedWitnesses) (swapHelp)) lst --swapt solange ein ungeordnetes Element, bis es keine ungeordneten gibt; braucht Hilfsfunktion, damit sich unorderedWitnesses entsprechend aktualisiert

bubbleSort lst = program lst 
  where
    n = length lst
    program = 
        (repeat (n-1)
          (foreach [0 .. n-2] (if2 swapPred swapOp)) --sortiert n-1 mal die Liste durch einfaches vertauschen aller nebeneinanderliegenden elemente, die falsch herum liegen
        )
        
swapPred (lst,i) = (get (i+1) lst) < (get i lst) --wenn nächstes element kleiner als aktuelles (=> ungeordnet)
swapOp (lst,i) = ((swap i (i+1) lst), undefined) --tauscht zwei aufeinanderfolgende elemente an index i; gibt tupel zurück
