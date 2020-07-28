module AB2_5 where

import Prelude ()
import Types (BooleanFunction(BooleanFunction),Arity,eval,partialApply,arity)
import SPLib.Basic (Int, Bool(True,False), trace, undefined, error, if', not, (.), (==), (/=), (<), (&&), (||), (+), (-), (++))
import SPLib.List (length, take, repeat)


sat  :: BooleanFunction -> Bool -- true iff the formula has at least one satisfying assignment
taut :: BooleanFunction -> Bool -- true iff all assignments satisfy the formula
       
-- *** DO NOT MODIFY ABOVE CODE ***

sat = undefined
taut = undefined