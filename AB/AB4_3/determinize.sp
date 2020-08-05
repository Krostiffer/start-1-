#NAME
determinize

#FUNCS
determinize' alphabet nfa = dfa

#VARS
alphabet :: String 
nfa      :: NFA

dfa :: DFA

cur_state : [State]

states_list :: [[State]]
next_states :: [[State]]
next_tuple :: []
work_list :: [[State]]

end_states :: [State]

delta_fun :: []

#PREDS
-- True, if no new states are found in next_states
p_inList = foldl (&&) True (map (noNewStates states_list) next_states)
p_isEndState = not (isEmpty (filter isFinalState cur_state))


#OPS
o_init:
    cur_state' = [startState nfa]
    states_list' = [[z0]]
    next_states = [[]]
    work_list' = [[]]
    end_states = []

o_nextStates:
    next_states' = map (nextStates nfa cur_state) alphabet
    delta_fun' = delta_fun ++ (map (\x-> (((listToState cur_state),x),(listToState (nextStates nfa cur_state x)))) alphabet)


o_updateStatesList:
    states_list' = removeDuplicates (states_list ++ next_states)
    work_list' = removeDuplicates (work_list ++ next_states)

o_updateCurState:
    cur_state' = first work_list
    work_list' = removeFirst work_list

o_updateEndState:
    end_states' = end_states ++ cur_state

o_makeDFA:
    DFA' = DFA (startState nfa) end_states (fromList delta_fun)
#FLOW
o_init = o_nextStates
o_nextStates = (p_inList o_makeDFA o_updateStatesList)
o_updateStatesList = (p_isEndState o_updateEndState o_updateCurState)
o_updateEndState = o_updateCurState
o_updateCurState = o_nextStates
o_makeDFA = HALT