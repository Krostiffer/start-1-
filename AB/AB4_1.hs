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


statePath a s i = if' ((length i) == 0) [s] (prepend s (statePath a (nextState a s (first i)) (removeFirst i)))
-- map takes arguments in the wrong order; help function to fix this issue
-- Creates a new Tree for every State in the nextstate-List.
traverseNextStates :: NFA -> String -> State -> Tree State
traverseNextStates a input state = stateTree a state (removeFirst input)

-- Make a tree with the cur_state as the node and the nextstates as children
-- stops when input is read (similar to statePath)
stateTree a cur_state input = 
  if' (isEmpty input) 
  (
    Tree cur_state []
  )(
    Tree cur_state (map (traverseNextStates a input) (nextStates a cur_state (first input)))
  )

-- teststatePath = statePath a1 "z0" "a"
-- tst = stateTree a3 "z0" "001"

-- the last state returned by the statePath-Function has to be checked for acceptance
inLanguageDFA a i = isFinalState a (last (statePath a (startState a) (i)))


-- returns True, if one of the given states is a final state
recOr :: NFA -> [State] -> Bool
recOr a states = 
  if' (isEmpty states) 
  (
    False
  )(
    isFinalState a (first states) || recOr a (removeFirst states)
  )


-- returns the list of next states for a given list of states with String i
nextStateList :: NFA -> String -> [State] -> [State]
nextStateList a i z = 
  if' (length i == 0)
    (
      z
    )(
      nextStateList a (removeFirst i) (nextList a i z)
    )

-- returns the list of next states for a given list of states for first symbol in i (since we have no CHAR)
nextList :: NFA -> String -> [State] -> [State]
nextList a i z = 
  if' (isEmpty z) 
  (
    []
  )(
    concat (map (\x -> nextStates a x (first i)) z)
  )
-- tstnxtstlst = nextStateList a3 "001" ["z0"]


-- checks if the last state in the taken path is undefined
isTotal a input = (last (statePath a (startState a) input)) /= "UNDEFINED!"
