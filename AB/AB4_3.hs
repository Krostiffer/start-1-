module AB4_3 where

import Prelude ()
import Types (DFA(DFA), NFA, DeltaFun, State, nextStates, startState, isFinalState)
import SPLib.Basic (Show(show), Bool(True,False), String, read, undefined, if', not, fst, snd, (+), (-), (&&), (.), (++),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt
  )
import SPLib.List (sort, removeDuplicates, filter, map, foldl, first, removeFirst, prepend, last, removeLast, append, isEmpty, concat, contains)
import SPLib.Map (fromList, set, empty)
import Testing --remove after testing

determinize  :: String -> NFA -> DFA

  
-- *** DO NOT MODIFY ABOVE CODE **

determinize = determinize'




--START OF PROGRAM determinize

data Data_determinize  = Data_determinize 
  String --alphabet
  [State] --cur_state
  DeltaFun --delta_fun
  DFA --dfa
  [State] --end_states
  [[State]] --next_states
  NFA --nfa
  [[State]] --states_list
  [[State]] --work_list
  Bool Bool Bool Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
determinize' :: String -> NFA -> DFA

determinize_o_init :: Data_determinize -> Data_determinize
determinize_o_makeDFA :: Data_determinize -> Data_determinize
determinize_o_nextStates :: Data_determinize -> Data_determinize
determinize_o_updateCurState :: Data_determinize -> Data_determinize
determinize_o_updateEndState :: Data_determinize -> Data_determinize
determinize_o_updateStatesList :: Data_determinize -> Data_determinize

determinize_p_inList :: Data_determinize -> Bool
determinize_p_isEndState :: Data_determinize -> Bool

--DEFINITIONS
determinize' alphabet nfa = (traceProgramCall "determinize'" "determinize" [("alphabet","String"),("cur_state","[State]"),("delta_fun","DeltaFun"),("dfa","DFA"),("end_states","[State]"),("next_states","[[State]]"),("nfa","NFA"),("states_list","[[State]]"),("work_list","[[State]]")]  dfa')
  where
    (Data_determinize _ _ _ dfa' _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (determinize_o_init (Data_determinize alphabet undefined undefined undefined undefined undefined nfa undefined undefined True False False False False False True False False))

determinize_p_inList (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list _ _ _ _ _ _ _ _ _) = foldl (&&) True (map (noNewStates states_list) next_states)

determinize_p_isEndState (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list _ _ _ _ _ _ _ _ _) = not (isEmpty (filter (isFinalState nfa) cur_state))

determinize_o_init (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_init" data_ [False,True,True,False,True,True,False,True,True] flow_)
  where
    cur_state' = [startState nfa]
    delta_fun' = empty
    end_states' = []
    next_states' = [[]]
    states_list' = [[startState nfa]]
    work_list' = [[]]
    alphabet' = alphabet
    dfa' = dfa
    nfa' = nfa
    cur_state_iDn_ = True
    delta_fun_iDn_ = True
    end_states_iDn_ = True
    next_states_iDn_ = True
    states_list_iDn_ = True
    work_list_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    dfa_iDn_ = dfa_iD_
    nfa_iDn_ = nfa_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    
    flow_ = 
      (traceProgramNextOp "determinize" "o_nextStates" (determinize_o_nextStates data_))

determinize_o_makeDFA (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_makeDFA" data_ [False,False,False,True,False,False,False,False,False] flow_)
  where
    dfa' = DFA (startState nfa) end_states (delta_fun)
    alphabet' = alphabet
    cur_state' = cur_state
    delta_fun' = delta_fun
    end_states' = end_states
    next_states' = next_states
    nfa' = nfa
    states_list' = states_list
    work_list' = work_list
    dfa_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    cur_state_iDn_ = cur_state_iD_
    delta_fun_iDn_ = delta_fun_iD_
    end_states_iDn_ = end_states_iD_
    next_states_iDn_ = next_states_iD_
    nfa_iDn_ = nfa_iD_
    states_list_iDn_ = states_list_iD_
    work_list_iDn_ = work_list_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    
    flow_ = 
      (traceProgramHalt "determinize" data_)

determinize_o_nextStates (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_nextStates" data_ [False,False,True,False,False,True,False,False,False] flow_)
  where
    delta_fun' = mapToDelta (map (nextList nfa cur_state) alphabet) cur_state alphabet delta_fun
    next_states' = map (nextList nfa cur_state) alphabet
    alphabet' = alphabet
    cur_state' = cur_state
    dfa' = dfa
    end_states' = end_states
    nfa' = nfa
    states_list' = states_list
    work_list' = work_list
    delta_fun_iDn_ = True
    next_states_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    cur_state_iDn_ = cur_state_iD_
    dfa_iDn_ = dfa_iD_
    end_states_iDn_ = end_states_iD_
    nfa_iDn_ = nfa_iD_
    states_list_iDn_ = states_list_iD_
    work_list_iDn_ = work_list_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    p__ = (determinize_p_inList data_)    
    flow_ = 
      (traceProgramPred "determinize" "p_inList" p__ (if' p__
        (traceProgramNextOp "determinize" "o_makeDFA" (determinize_o_makeDFA data_))
        (traceProgramNextOp "determinize" "o_updateStatesList" (determinize_o_updateStatesList data_)) 
      ))

determinize_o_updateCurState (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_updateCurState" data_ [False,True,False,False,False,False,False,False,True] flow_)
  where
    cur_state' = first work_list
    work_list' = removeFirst work_list
    alphabet' = alphabet
    delta_fun' = delta_fun
    dfa' = dfa
    end_states' = end_states
    next_states' = next_states
    nfa' = nfa
    states_list' = states_list
    cur_state_iDn_ = True
    work_list_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    delta_fun_iDn_ = delta_fun_iD_
    dfa_iDn_ = dfa_iD_
    end_states_iDn_ = end_states_iD_
    next_states_iDn_ = next_states_iD_
    nfa_iDn_ = nfa_iD_
    states_list_iDn_ = states_list_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    
    flow_ = 
      (traceProgramNextOp "determinize" "o_nextStates" (determinize_o_nextStates data_))

determinize_o_updateEndState (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_updateEndState" data_ [False,False,False,False,True,False,False,False,False] flow_)
  where
    end_states' = end_states ++ cur_state
    alphabet' = alphabet
    cur_state' = cur_state
    delta_fun' = delta_fun
    dfa' = dfa
    next_states' = next_states
    nfa' = nfa
    states_list' = states_list
    work_list' = work_list
    end_states_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    cur_state_iDn_ = cur_state_iD_
    delta_fun_iDn_ = delta_fun_iD_
    dfa_iDn_ = dfa_iD_
    next_states_iDn_ = next_states_iD_
    nfa_iDn_ = nfa_iD_
    states_list_iDn_ = states_list_iD_
    work_list_iDn_ = work_list_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    
    flow_ = 
      (traceProgramNextOp "determinize" "o_updateCurState" (determinize_o_updateCurState data_))

determinize_o_updateStatesList (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = (traceProgramOp "determinize" "o_updateStatesList" data_ [False,False,False,False,False,False,False,True,True] flow_)
  where
    states_list' = removeDuplicates (states_list ++ next_states)
    work_list' = removeDuplicates (work_list ++ next_states)
    alphabet' = alphabet
    cur_state' = cur_state
    delta_fun' = delta_fun
    dfa' = dfa
    end_states' = end_states
    next_states' = next_states
    nfa' = nfa
    states_list_iDn_ = True
    work_list_iDn_ = True
    alphabet_iDn_ = alphabet_iD_
    cur_state_iDn_ = cur_state_iD_
    delta_fun_iDn_ = delta_fun_iD_
    dfa_iDn_ = dfa_iD_
    end_states_iDn_ = end_states_iD_
    next_states_iDn_ = next_states_iD_
    nfa_iDn_ = nfa_iD_
    data_ = (Data_determinize alphabet' cur_state' delta_fun' dfa' end_states' next_states' nfa' states_list' work_list' alphabet_iDn_ cur_state_iDn_ delta_fun_iDn_ dfa_iDn_ end_states_iDn_ next_states_iDn_ nfa_iDn_ states_list_iDn_ work_list_iDn_)
    p__ = (determinize_p_isEndState data_)    
    flow_ = 
      (traceProgramPred "determinize" "p_isEndState" p__ (if' p__
        (traceProgramNextOp "determinize" "o_updateEndState" (determinize_o_updateEndState data_))
        (traceProgramNextOp "determinize" "o_updateCurState" (determinize_o_updateCurState data_)) 
      ))

--SHOW
instance Show Data_determinize where  
  show (Data_determinize alphabet cur_state delta_fun dfa end_states next_states nfa states_list work_list alphabet_iD_ cur_state_iD_ delta_fun_iD_ dfa_iD_ end_states_iD_ next_states_iD_ nfa_iD_ states_list_iD_ work_list_iD_) = 
    (show [
      (if' alphabet_iD_ (prepend 'd' (show alphabet)) "u"),
      (if' cur_state_iD_ (prepend 'd' (show cur_state)) "u"),
      (if' delta_fun_iD_ (prepend 'd' (show delta_fun)) "u"),
      (if' dfa_iD_ (prepend 'd' (show dfa)) "u"),
      (if' end_states_iD_ (prepend 'd' (show end_states)) "u"),
      (if' next_states_iD_ (prepend 'd' (show next_states)) "u"),
      (if' nfa_iD_ (prepend 'd' (show nfa)) "u"),
      (if' states_list_iD_ (prepend 'd' (show states_list)) "u"),
      (if' work_list_iD_ (prepend 'd' (show work_list)) "u")
    ])

--END OF PROGRAM determinize

noNewStates :: [[State]] -> [State] -> Bool
noNewStates states state = contains (listToState state) (map listToState states)


nextList a z i=
  if'(isEmpty z)
    ( 
      []
    )( 
      concat (map (\x -> nextStates a x i) z)
    )

listToState :: [State] -> String
listToState states = foldl (++) "" states 
mapToDelta neSt cur_state alphab delta = 
  if' (isEmpty (removeFirst alphab))
    (set (listToState (first neSt)) ((listToState cur_state), first alphab) delta)
    (mapToDelta (removeFirst neSt) cur_state (removeFirst alphab) (set (listToState (first neSt)) ((listToState cur_state), first alphab) delta))


nfa1 = Testing.m2

testtest = determinize "01" nfa1

