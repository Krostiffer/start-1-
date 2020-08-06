#NAME
partition

#FUNCS
partition_list'  list start end pivot = list
partition_pivot' list start end pivot = pivot 

#VARS
list    :: [Int]
start   :: Int   --start index of virtual list
end     :: Int   --end index of virtual list
pivot   :: Int   --index of pivot element

cur     :: Int   --index of current element to be compared 
counter :: Int   --number of elements compared to pivot

#PREDS
p_validList = (length list) > 1
p_validCount = counter > 0
p_greaterPivot = pivot > cur
p_swap = (get pivot list) <= (get cur list) -- if pivot > cur
p_swap2 = (get pivot list) > (get cur list) -- if pivot <= cur
p_swapTwice = (get pivot list) <= (get (pivot + 1) list) -- check only if pivot <= cur

#OPS
o_init:
  cur' = start
  counter' = (end - start)

o_move:

o_swap:
  list' = (swap cur pivot list)
  pivot' = cur
  cur' = cur + 1
  counter' = counter - 1

o_swapTwice:
  list' = (swap cur (pivot + 1) (swap cur pivot list))
  pivot' = pivot + 1
  cur' = cur + 1
  counter' = counter-1

o_skip:
  cur' = cur + 1
  counter' = counter - 1

#FLOW
o_init = (p_validList (o_move) (HALT)) -- If the list only has one element, return the list.
-- For the following, the flow is the same.
-- First check if the counter is valid, if not HALT.
-- Secondly check if pivot is greater than current index.
-- 	- if true, check if the pivot-element is smaller than the index-element and swap if true.
--  - if not, check if the pivot-element is greater than the index-element and skip if not.
--		- if true, further check if the pivot+1-element is greater than the current pivot element.
--			- if true, swap twice (if you only swap once, we might have a greater element inbetween)
--			- if not, swap only once.
o_move = (p_validCount 
            (p_greaterPivot 
                (p_swap (o_swap) (o_skip)) 
                (p_swap2 
                    (p_swapTwice (o_swapTwice) (o_swap)) 
                    (o_skip))) 
          (HALT))
o_swap = (p_validCount (p_greaterPivot (p_swap (o_swap) (o_skip)) (p_swap2 (p_swapTwice (o_swapTwice) (o_swap)) (o_skip))) (HALT))
o_swapTwice = (p_validCount (p_greaterPivot (p_swap (o_swap) (o_skip)) (p_swap2 (p_swapTwice (o_swapTwice) (o_swap)) (o_skip))) (HALT))
o_skip = (p_validCount (p_greaterPivot (p_swap (o_swap) (o_skip)) (p_swap2 (p_swapTwice (o_swapTwice) (o_swap)) (o_skip))) (HALT))
