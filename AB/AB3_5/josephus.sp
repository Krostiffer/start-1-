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
p_last = (length circle) == 1

#OPS
o_init:
  circle' = [1..n]
  cur' = 0

o_remove:
  circle' = if' ((length circle) == (cur + 1)) (remove 0 circle) (remove (cur + 1) circle)
  cur' =  if' ((length circle) == (cur + 2)) 0 (if' ((length circle) == (cur + 1)) 0 (cur + 1))
  
o_last:
  last' = (first circle)
  
#FLOW
o_init = (p_last o_last o_remove)
o_remove = (p_last o_last o_remove)
o_last = HALT