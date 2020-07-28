module AB2_1 where

import Prelude ()
import SPLib.Basic (Ord, Int, trace, undefined, if', (<), (+), (-))
import SPLib.List (first, isEmpty, length, get, set, filter)


unorderedWitnesses :: (Ord a) => [a] -> [Int] --returns list of indices i s.t. A[i+1] < A[i]
swap :: Int -> Int -> [a] -> [a] --swaps i-th and j-th element in list
primitiveSort :: (Ord a) => [a] -> [a]

-- *** DO NOT MODIFY ABOVE CODE ***

--erstellt eine Liste mit allen indizes und filtert diese wie oben angegeben
unorderedWitnesses a = filter f [0..((length a) -2)]
    where
        f x = (get (x+1) a) < (get (x) a)

-- da set die geänderte Liste ausgibt, wird mit set(set()) zwei Änderungen hintereinander realisiert
swap i j a = set (get j a) i (set (get i a) j a)

primitiveSort a = recSort a 0 (unorderedWitnesses a)
-- swapt rekursiv die Liste an den Stellen der Witnesses. Läuft mehrfach durch, bis keine Witnesses mehr vorhanden sind
recSort a i w = if' (i < length w) goThroughWitnesses untilWitnessesEmpty
    where 
        goThroughWitnesses = recSort (swap (get i w) ((get i w) + 1) a) (i + 1) w --swapt alle witnesses, solange welche da sind
        untilWitnessesEmpty = if' (0 < length w) (recSort a 0 (unorderedWitnesses a)) a --startet das swappen von vorn, sofern es noch unordered witnesses gibt