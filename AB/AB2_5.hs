module AB2_5 where

import Prelude ()
import Types (BooleanFunction(BooleanFunction),Arity,eval,partialApply,arity)
import SPLib.Basic (Int, Bool(True,False), trace, undefined, error, if', not, (.), (==), (/=), (<), (&&), (||), (+), (-), (++))
import SPLib.List (length, take, repeat)


sat  :: BooleanFunction -> Bool -- true iff the formula has at least one satisfying assignment
taut :: BooleanFunction -> Bool -- true iff all assignments satisfy the formula
       
-- *** DO NOT MODIFY ABOVE CODE ***

--geht rekursiv durch die Formel durch, bis keine Variable mehr vorhanden (alle gesetzt); Bei sat rekursiv true oder false, da nur eine belegung benötigt wird; Bei taut und, da alle belegungen benötigt werden
sat bf = if' ((arity bf) == 0) (eval [] bf) (sat (partialApply False bf) || sat (partialApply True bf))
taut bf = if' ((arity bf) == 0) (eval [] bf) (taut (partialApply False bf) && taut (partialApply True bf))