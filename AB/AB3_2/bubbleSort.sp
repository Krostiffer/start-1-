#NAME 
bubbleSort

#FUNCS
bubbleSort' list = list 

#VARS 
list   :: [Int]
cur    :: Int
passes :: Int   --number of iterations over the list
sorted :: Bool  --true at the end of an iteration iff list is sorted

#PREDS
p_lt2  = ?
p_swap = ?
p_endOfIteration = ?
p_nextIteration = ?

#OPS
o_init:  
  cur' = 0
  passes' = 0
  sorted' = ?
  
o_move:
  cur' = cur + 1

o_swap:
  list' = (swap cur (cur+1) list)
  cur' = cur + 1
  sorted' = ?

o_next:
  cur' = 0 
  passes' = passes + 1
  sorted' = ?
  
#FLOW
o_init = (p_lt2 HALT (p_swap o_swap o_move))
o_move = (p_endOfIteration (p_nextIteration o_next HALT) (p_swap o_swap o_move))
o_swap = (p_endOfIteration (p_nextIteration o_next HALT) (p_swap o_swap o_move))
o_next = (p_swap o_swap o_move)
