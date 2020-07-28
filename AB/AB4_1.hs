module AB4_1 where

import Prelude ()
import Types (NFA, DFA, State, isUndefinedTransition, nextState, nextStates, startState, isFinalState, states)
import AB1_3 (crossList)
import SPLib.Basic (Bool(True,False), String, trace, undefined, if', not, (&&), (||), (.), (+), (-), (==), (/=))
import SPLib.List (map, filter, foldl, first, removeFirst, prepend, last, concat, isEmpty, length)
import SPLib.Tree (Tree(Tree))


class FormalLanguage a where
  inLanguage :: a -> String -> Bool

instance FormalLanguage DFA where
  inLanguage = inLanguageDFA 

instance FormalLanguage NFA where
  inLanguage = inLanguageNFA

statePath :: DFA -> State -> String -> [State]
stateTree :: NFA -> State -> String -> Tree State 

inLanguageDFA :: DFA -> String -> Bool
inLanguageNFA :: NFA -> String -> Bool

isTotal :: DFA -> String -> Bool --true iff transition function is defined for all 
  
-- *** DO NOT MODIFY ABOVE CODE ***

statePath = undefined
stateTree = undefined

inLanguageDFA = undefined
inLanguageNFA = undefined

isTotal = undefined
