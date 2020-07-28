module AB4_2 where

import Prelude ()
import Types (DFA(DFA), State, nextState, startState, isFinalState, states)
import AB1_3 (crossList)
import SPLib.Basic (Show(show), Bool(True,False), String, trace, undefined, if', not, (+), (-), (&&), (.))
import SPLib.List (filter, map, foldl, prepend, get, length)
import SPLib.Map (fromList, set, empty)


single :: String -> DFA --returns DFA which only accepts given string

complement   :: String -> DFA -> DFA 
union        :: String -> DFA -> DFA -> DFA 
intersection :: String -> DFA -> DFA -> DFA
  
-- *** DO NOT MODIFY ABOVE CODE ***

single = undefined
 
complement = undefined
union = undefined
intersection = undefined
 