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
p_last = ?

#OPS
o_init:
  circle' = [1..n]
  cur' = ?

o_remove:
  circle' = ?
  cur' = ?
  
o_last:
  last' = ?
  
#FLOW
o_init = ?
o_remove = ?
o_last = ?