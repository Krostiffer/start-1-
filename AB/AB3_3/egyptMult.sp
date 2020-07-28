#NAME 
egyptMult

#FUNCS
egyptMult' a b = r

#VARS 
a :: Int
b :: Int
i :: Int
c :: Int
r :: Int

#PREDS
p_addEven = ?
p_addOdd = ?
p_even = ?
p_end = ?

#OPS
o_init:
  r' = ?
  c' = ?
  i' = ?

o_addEven:  
  i' = ?
  c' = ?
  r' = ?

o_addOdd:  
  i' = ?
  c' = ?
  r' = ?
  
o_skipEven:  
  i' = ?
  c' = ?
  
o_skipOdd:  
  i' = ?
  c' = ?
  
#FLOW
o_init = ?
o_addEven = ?
o_addOdd = ?
o_skipEven = ?
o_skipOdd = ?
