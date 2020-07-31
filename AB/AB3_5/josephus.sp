#NAME 
josephus

#FUNCS
josephus' n = last

#VARS 
n      :: Int
last   :: Int 
circle :: [Int]
cur    :: Int --index of current person in circle

#PREDS
p_last = (length circle) == 1 --if just one is left, this is the last one; obviously

#OPS
o_init:
  circle' = [1..n] 
  cur' = 0 --sets index to 0 (starts with number 1)

o_remove:
  circle' = if' ((length circle) == (cur + 1)) (remove 0 circle) (remove (cur + 1) circle) --when cur = maxIndex we remove the first element of the list, else just remove the next element
  cur' =  if' ((length circle) == (cur + 2)) 0 (if' ((length circle) == (cur + 1)) 0 (cur + 1)) --if cur = maxIndex -1 jumps to beginning of list since the last element just got deleted, if cur = maxIndex naturally jump to beginning of the list, else jump to next index
  
o_last:
  last' = (first circle) -- returns the first (and only) element of the list (which is always length 1)
  
#FLOW
o_init = (p_last o_last o_remove)   -- \ checks if just one element is remaining, else continues with removal
o_remove = (p_last o_last o_remove) -- / same
o_last = HALT --halts if just one is remaining