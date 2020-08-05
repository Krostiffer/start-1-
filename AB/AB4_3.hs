module AB4_3 where

import Prelude ()
import Types (DFA(DFA), NFA, DeltaFun, State, nextStates, startState, isFinalState)
import SPLib.Basic (Show(show), Bool(True,False), String, read, undefined, if', not, fst, snd, (+), (-), (&&), (.), (++),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt
  )
import SPLib.List (sort, removeDuplicates, filter, map, foldl, first, removeFirst, prepend, last, removeLast, append, isEmpty, concat, contains)
import SPLib.Map (fromList, set, empty)


determinize  :: String -> NFA -> DFA

  
-- *** DO NOT MODIFY ABOVE CODE **

noNewStates :: [State] -> State -> Bool
noNewStates states state = contains state states

listToState :: [State] -> String
listToState states = foldl (++) "" states 

determinize = undefined