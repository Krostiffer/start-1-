#NAME
select

#FUNCS
select' i list = res

#VARS
list  :: [Int] --non-empty list
i     :: Int   --select i-th smallest element (i>=1)
start :: Int
end   :: Int
pivot :: Int

res :: Int

#PREDS
p_elementFound = pivot == i
p_checkElement = pivot > i

#OPS
-- Sets i (i-1, becaus we need the index), start index and end index.
o_init:
  i' = (i-1)
  end' = (length list)-1
  start' = 0

-- Partition the list with the pivot element as the last element of the given list and setting the pivot-index.
o_firstSearch:
  list' = (partition_list list start end end)
  pivot' = (partition_pivot list start end end)

-- When partitioning the left list, set the end index to pivot-1.
o_searchLeft:
  end' = (pivot-1)
  list' = (partition_list list start (pivot-1) (pivot-1))
  pivot' = (partition_pivot list start (pivot-1) (pivot-1))
  
-- When partitioning the right list, set the start index to pivot+1.
o_searchRight:
  start' = (pivot+1)
  list' = (partition_list list (pivot+1) end end)
  pivot' = (partition_pivot list (pivot+1) end end)

o_found:
  res' = (get pivot list)

#FLOW
o_init = (o_firstSearch) -- Initialize the i-th element to serach for aswell as the start and end indices
-- For the following, the flow is the same.
-- First, partition the list with the last element in the list as the pivot-element.
-- If the pivot element is the i-th element we are searching for, set the pivot-element as res and HALT.
-- If not, depending on wether the pivot-index is greater or smaller than i, continue the search in the left list (pivot > i) or in the right list (pivot < i).
o_firstSearch = (p_elementFound
                   (o_found)
				   (p_checkElement
				     (o_searchLeft)
					 (o_searchRight)))
o_searchLeft = (p_elementFound (o_found) (p_checkElement (o_searchLeft) (o_searchRight)))
o_searchRight = (p_elementFound (o_found) (p_checkElement (o_searchLeft) (o_searchRight)))
o_found = HALT
