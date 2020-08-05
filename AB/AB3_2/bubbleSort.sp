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
p_lt2  = (length list) < 2 -- if length <= 1 no sorting can be done.
p_swap = (get (cur +1) list) < (get cur list) --swapPred
p_endOfIteration = (length list) < ((cur +2) + passes) --iteration ends when cur is at length lsit
p_nextIteration = (not sorted) && ((passes +2) < (length list)) --needs next iteration when not sorted and passes+1 < maxIndex

#OPS
o_init:  --initialisation
  cur' = 0
  passes' = 0 
  sorted' = True --only gets changed if list gets sorted
  
o_move:
  cur' = cur + 1 --move 1 forward, nothing else

o_swap:
  list' = (swap cur (cur+1) list) --swaps list element with successor
  cur' = cur + 1 --move 1 forward
  sorted' = False --list got sorted, so it wasn't sorted which means sorted is false

o_next:
  cur' = 0  --element index gets reset
  passes' = passes + 1 -- passes gets increased, so further iterations need 1 step less
  sorted' = True
  
#FLOW
o_init = (p_lt2 HALT (p_swap o_swap o_move)) --halts if l <= 1 
o_move = (p_endOfIteration (p_nextIteration o_next HALT) (p_swap o_swap o_move)) -- \ checks for end of iteration; if ended, checks if there is a next iteration needed, Halts if not needed, does one if needed. When not ended, continues swapping or moving
o_swap = (p_endOfIteration (p_nextIteration o_next HALT) (p_swap o_swap o_move)) -- / the same: moving or swapping doesn't directly affect next steps
o_next = (p_swap o_swap o_move) --starts a new iteration with either swapping or moving