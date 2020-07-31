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
p_addEven = (mod (div i 2) 2) == 1
p_addOdd = (mod (div i 2) 2) == 1
p_even = (mod i 2) == 0
p_end = (i == 1) || (i == 0)

#OPS
o_init:
  r' = if' ((mod b 2) == 0) 0 a
  c' = a
  i' = b

o_addEven:  
  i' = (div i 2)
  c' = (c + c)
  r' = (r + c + c)

o_addOdd:  
  i' = (div i 2)
  c' = (c + c)
  r' = (r + c + c)
  
o_skipEven:  
  i' = (div i 2)
  c' = (c + c)
  
o_skipOdd:  
  i' = (div i 2)
  c' = (c + c)
  
#FLOW
o_init = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_addEven = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_addOdd = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_skipEven = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_skipOdd = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
