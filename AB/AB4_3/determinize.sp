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
p_noNewStates = (foldl (&&) True (map (noNewStates states_list) next_states) && (isEmpty work_list))
-- True if one of the underlying states of the current state (x1 or x2 from State [x1x2]) is a final state in the nfa
p_isEndState = not (isEmpty (filter (isFinalState nfa) cur_state))


#OPS
--initialization 
o_init:
    cur_state' = [startState nfa] 
    states_list' = [[startState nfa]] -- aka already-done-list
    next_states' = [[]] 
    work_list' = [] --aka Still-to-do list
    end_states' = []
    delta_fun' = empty

--sets the next states and delta function based on current state and input
o_nextStates:
    next_states' = map (nextList nfa cur_state) alphabet
    delta_fun' = mapToDelta (map (nextList nfa cur_state) alphabet) cur_state alphabet delta_fun

-- adds all new states in the next states to the worklist
o_updateWorkList:
    work_list' = removeDuplicates (filter ((newStates states_list)) (work_list ++ next_states))

-- updates current state, statelist and worklist accordingly
o_updateCurState: 
    cur_state' = first work_list -- updates current state to be the first in the worklist
    states_list' = removeDuplicates (append (first work_list) states_list) --adds the new current state to the statelist
    work_list' = removeFirst work_list -- remove the current state from the worklist

--adds the current state to endStates (in flow: only if it is an endstate)
o_updateEndState:
    end_states' = append (listToState cur_state) end_states
-- the same as above, just used in once before halting
o_end:
    end_states' = append (listToState cur_state) end_states
--creates the fininshed DFA
o_makeDFA:
    dfa' = DFA (startState nfa) end_states delta_fun

#FLOW
o_init = (o_nextStates) 
o_nextStates = (p_noNewStates (p_isEndState o_end o_makeDFA) o_updateWorkList) --if no new states are found, checks current state for endstate else go on with next state
o_updateWorkList = (p_isEndState o_updateEndState o_updateCurState) --checks if endstate -> adds the current state to endstates, then continues, else just continue
o_updateEndState = (o_updateCurState) --continues
o_updateCurState = (o_nextStates) --updates current state and start "loop" again
o_end = o_makeDFA --as mentioned above, the same as updateEndState but just once before halting
o_makeDFA = (HALT) --The End