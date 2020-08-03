module AB4_1 where

import Prelude ()
import Types (NFA, DFA, State, isUndefinedTransition, nextState, nextStates, startState, isFinalState, states)
import AB1_3 (crossList)
import SPLib.Basic (Bool(True,False), String, trace, undefined, if', not, (&&), (||), (.), (+), (-), (==), (/=))
import SPLib.List (map, filter, foldl, first, removeFirst, prepend, last, concat, isEmpty, length)
import SPLib.Tree (Tree(Tree))

-- REMOVE AFTER TESTING
import Testing


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
a1 :: DFA
a1 = Testing.m1

a2 :: NFA
a2 = Testing.m2

statePath a cur_state input = if' ((length input) == 0) [cur_state] (prepend cur_state (statePath a (nextState a cur_state (first input)) (removeFirst input)))
stateTree = undefined

inLanguageDFA a input = isFinalState a (last (statePath a (startState a) (input)))
inLanguageNFA = undefined

isTotal = undefined
