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
o_init:
  i' = (i-1)
  start' = 0
  end' = ((length list)-1)

o_firstSearch:
  list' = (partition_list list start end end)
  pivot' = (partition_pivot list start end end)

o_searchLeft:
  end' = (pivot-1)
  list' = (partition_list list start (pivot-1) (pivot-1))
  pivot' = (partition_pivot list start (pivot-1) (pivot-1))
  
o_searchRight:
  start' = (pivot+1)
  list' = (partition_list list (pivot+1) end end)
  pivot' = (partition_pivot list (pivot+1) end end)

o_found:
  res' = (get pivot list)

#FLOW
o_init = (o_firstSearch)
o_firstSearch = (p_elementFound (o_found) (p_checkElement (o_searchLeft) (o_searchRight)))
o_searchLeft = (p_elementFound (o_found) (p_checkElement (o_searchLeft) (o_searchRight)))
o_searchRight = (p_elementFound (o_found) (p_checkElement (o_searchLeft) (o_searchRight)))
o_found = HALT