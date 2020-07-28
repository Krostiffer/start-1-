module AB4_4 where

import Prelude ()
import Types (DFA(DFA), NFA(NFA), DeltaFun, State)
import AB4_3 (determinize)
import SPLib.Basic 
  (Show(show), Bool(True,False), Int, String, trace, undefined, if', not, read, fst, snd, (+), (-), (&&), (.), (==), (++), 
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List as L (sort, removeDuplicates, filter, map, foldl, prepend, get, length, first, removeFirst, last, removeLast, prepend, append, isEmpty, concat, contains)
import SPLib.Map as M (fromList, toList, get, set, empty)


t9dfa :: [String] -> DFA

-- *** DO NOT MODIFY ABOVE CODE ***

t9dfa = undefined