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

-- a1 = Testing.m1
-- a2 = Testing.m2
-- a3 = Testing.m3
statePath a s i = if' ((length i) == 1) [(nextState a s (first i))] (prepend s (statePath a (nextState a s (first i)) (removeFirst i)))
traverseNextStates a input state = stateTree a state (removeFirst input)
stateTree a cur_state input = 
  if' (isEmpty input) 
  (
    Tree cur_state []
  )(
    Tree cur_state (map (traverseNextStates a input) (nextStates a cur_state (first input)))
  )

-- teststatePath = statePath a1 "z0" "a"
-- tst = stateTree a3 "z0" "001"

inLanguageDFA a i = isFinalState a (last (statePath a (startState a) (i)))
recOr a h = 
  if' (isEmpty h) 
  (
    False
  )(
    isFinalState a (first h) || recOr a (removeFirst h)
  )

inLanguageNFA a i = recOr a (nextStateList a i [startState a])

nextStateList :: NFA -> String -> [State] -> [State]
nextStateList a i z = 
  if' (length i == 1)
    (
      nextList a i z
    )(
      nextStateList a (removeFirst i) (nextList a i z)
    )

nextList a i z = 
  if' (isEmpty z) 
  (
    []
  )(
    concat (map (\x -> nextStates a x (first i)) z)
  )
-- tstnxtstlst = nextStateList a3 "001" ["z0"]

isTotal a input = (last (statePath a (startState a) input)) /= "UNDEFINED!"
