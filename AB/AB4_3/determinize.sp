#NAME
determinize

#FUNCS
determinize' alphabet nfa = dfa

#VARS
alphabet :: String 
nfa      :: NFA

dfa :: DFA

cur_state :: [State]

states_list :: [[State]]
next_states :: [[State]]
work_list :: [[State]]

end_states :: [State]

delta_fun :: DeltaFun

#PREDS
-- True, if no new states are found in next_states
p_inList = foldl (&&) True (map (noNewStates states_list) next_states)
p_isEndState = not (isEmpty (filter (isFinalState nfa) cur_state)) 


#OPS
o_init:
    cur_state' = [startState nfa]
    states_list' = [[startState nfa]]
    next_states' = [[]]
    work_list' = [[]]
    end_states' = []
    delta_fun' = empty

o_nextStates:
    next_states' = map (nextList nfa cur_state) alphabet
    delta_fun' = mapToDelta (map (nextList nfa cur_state) alphabet) cur_state alphabet delta_fun


o_updateStatesList:
    states_list' = removeDuplicates (states_list ++ next_states)
    work_list' = removeDuplicates (work_list ++ next_states)

o_updateCurState:
    cur_state' = first work_list
    work_list' = removeFirst work_list

o_updateEndState:
    end_states' = end_states ++ cur_state 

o_makeDFA:
    dfa' = DFA (startState nfa) end_states (delta_fun)
#FLOW
o_init = o_nextStates
o_nextStates = (p_inList o_makeDFA o_updateStatesList)
o_updateStatesList = (p_isEndState o_updateEndState o_updateCurState)
o_updateEndState = o_updateCurState
o_updateCurState = o_nextStates
o_makeDFA = HALT