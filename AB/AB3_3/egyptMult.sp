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
p_addEven = (mod (div i 2) 2) == 1 -- if i in the next step is odd as that is the step where the addition to r has to take place
p_addOdd = (mod (div i 2) 2) == 1 -- see p_addEven
p_even = (mod i 2) == 0
p_end = (i == 1) || (i == 0) -- i == 0 for multiplications with 0

#OPS
o_init:
  r' = if' ((mod b 2) == 0) 0 a -- if the second factor is odd then set a as the rest, otherwise 0 (a=7,b=11 => r=7; a=5,b=16 => r=0)
  c' = a
  i' = b

o_addEven:  
  i' = (div i 2)
  c' = (c + c)
  r' = (r + c + c)

-- o_addOdd is the same as addEven as the Integer Division already rounds down
o_addOdd:  
  i' = (div i 2)
  c' = (c + c)
  r' = (r + c + c)
  
o_skipEven:  
  i' = (div i 2)
  c' = (c + c)

-- see o_addOdd
o_skipOdd:  
  i' = (div i 2)
  c' = (c + c)
  
#FLOW
-- every part can lead to the end of the calculation and thus is the same
o_init = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_addEven = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_addOdd = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_skipEven = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
o_skipOdd = (p_end HALT (p_even (p_addEven o_addEven o_skipEven) (p_addOdd o_addOdd o_skipOdd)))
