module AB5_1 where

import Prelude ()
import AB2_1 (swap)
import SPLib.Basic 
  (Show(show), Int, Bool(True,False), undefined, if', (+), (-), (<), (<=), (>), (>=), (==), 
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List (length, prepend, get)


partition_list  :: [Int] -> Int -> Int -> Int -> [Int]
partition_pivot :: [Int] -> Int -> Int -> Int -> Int
select :: Int -> [Int] -> Int

partition_list = partition_list' 
partition_pivot = partition_pivot' 
select = select'


-- *** DO NOT MODIFY ABOVE CODE ***

--START OF PROGRAM select

data Data_select  = Data_select 
  Int --end
  Int --i
  [Int] --list
  Int --pivot
  Int --res
  Int --start
  Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
select' :: Int -> [Int] -> Int

select_o_firstSearch :: Data_select -> Data_select
select_o_found :: Data_select -> Data_select
select_o_init :: Data_select -> Data_select
select_o_searchLeft :: Data_select -> Data_select
select_o_searchRight :: Data_select -> Data_select

select_p_checkElement :: Data_select -> Bool
select_p_elementFound :: Data_select -> Bool

--DEFINITIONS
select' i list = (traceProgramCall "select'" "select" [("end","Int"),("i","Int"),("list","[Int]"),("pivot","Int"),("res","Int"),("start","Int")]  res')
  where
    (Data_select _ _ _ _ res' _ _ _ _ _ _ _) = (select_o_init (Data_select undefined i list undefined undefined undefined False True True False False False))

select_p_checkElement (Data_select end i list pivot res start _ _ _ _ _ _) = pivot > i

select_p_elementFound (Data_select end i list pivot res start _ _ _ _ _ _) = pivot == i

select_o_firstSearch (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = (traceProgramOp "select" "o_firstSearch" data_ [False,False,True,True,False,False] flow_)
  where
    list' = (partition_list list start end end)
    pivot' = (partition_pivot list start end end)
    end' = end
    i' = i
    res' = res
    start' = start
    list_iDn_ = True
    pivot_iDn_ = True
    end_iDn_ = end_iD_
    i_iDn_ = i_iD_
    res_iDn_ = res_iD_
    start_iDn_ = start_iD_
    data_ = (Data_select end' i' list' pivot' res' start' end_iDn_ i_iDn_ list_iDn_ pivot_iDn_ res_iDn_ start_iDn_)
    p__ = (select_p_elementFound data_)
    p_1_ = (select_p_checkElement data_)    
    flow_ = 
      (traceProgramPred "select" "p_elementFound" p__ (if' p__
        (traceProgramNextOp "select" "o_found" (select_o_found data_))
        (traceProgramPred "select" "p_checkElement" p_1_ (if' p_1_
          (traceProgramNextOp "select" "o_searchLeft" (select_o_searchLeft data_))
          (traceProgramNextOp "select" "o_searchRight" (select_o_searchRight data_)) 
        )) 
      ))

select_o_found (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = (traceProgramOp "select" "o_found" data_ [False,False,False,False,True,False] flow_)
  where
    res' = (get pivot list)
    end' = end
    i' = i
    list' = list
    pivot' = pivot
    start' = start
    res_iDn_ = True
    end_iDn_ = end_iD_
    i_iDn_ = i_iD_
    list_iDn_ = list_iD_
    pivot_iDn_ = pivot_iD_
    start_iDn_ = start_iD_
    data_ = (Data_select end' i' list' pivot' res' start' end_iDn_ i_iDn_ list_iDn_ pivot_iDn_ res_iDn_ start_iDn_)
    
    flow_ = 
      (traceProgramHalt "select" data_)

select_o_init (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = (traceProgramOp "select" "o_init" data_ [True,True,False,False,False,True] flow_)
  where
    end' = ((length list)-1)
    i' = (i-1)
    start' = 0
    list' = list
    pivot' = pivot
    res' = res
    end_iDn_ = True
    i_iDn_ = True
    start_iDn_ = True
    list_iDn_ = list_iD_
    pivot_iDn_ = pivot_iD_
    res_iDn_ = res_iD_
    data_ = (Data_select end' i' list' pivot' res' start' end_iDn_ i_iDn_ list_iDn_ pivot_iDn_ res_iDn_ start_iDn_)
    
    flow_ = 
      (traceProgramNextOp "select" "o_firstSearch" (select_o_firstSearch data_))

select_o_searchLeft (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = (traceProgramOp "select" "o_searchLeft" data_ [True,False,True,True,False,False] flow_)
  where
    end' = (pivot-1)
    list' = (partition_list list start (pivot-1) (pivot-1))
    pivot' = (partition_pivot list start (pivot-1) (pivot-1))
    i' = i
    res' = res
    start' = start
    end_iDn_ = True
    list_iDn_ = True
    pivot_iDn_ = True
    i_iDn_ = i_iD_
    res_iDn_ = res_iD_
    start_iDn_ = start_iD_
    data_ = (Data_select end' i' list' pivot' res' start' end_iDn_ i_iDn_ list_iDn_ pivot_iDn_ res_iDn_ start_iDn_)
    p__ = (select_p_elementFound data_)
    p_1_ = (select_p_checkElement data_)    
    flow_ = 
      (traceProgramPred "select" "p_elementFound" p__ (if' p__
        (traceProgramNextOp "select" "o_found" (select_o_found data_))
        (traceProgramPred "select" "p_checkElement" p_1_ (if' p_1_
          (traceProgramNextOp "select" "o_searchLeft" (select_o_searchLeft data_))
          (traceProgramNextOp "select" "o_searchRight" (select_o_searchRight data_)) 
        )) 
      ))

select_o_searchRight (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = (traceProgramOp "select" "o_searchRight" data_ [False,False,True,True,False,True] flow_)
  where
    list' = (partition_list list (pivot+1) end end)
    pivot' = (partition_pivot list (pivot+1) end end)
    start' = (pivot+1)
    end' = end
    i' = i
    res' = res
    list_iDn_ = True
    pivot_iDn_ = True
    start_iDn_ = True
    end_iDn_ = end_iD_
    i_iDn_ = i_iD_
    res_iDn_ = res_iD_
    data_ = (Data_select end' i' list' pivot' res' start' end_iDn_ i_iDn_ list_iDn_ pivot_iDn_ res_iDn_ start_iDn_)
    p__ = (select_p_elementFound data_)
    p_1_ = (select_p_checkElement data_)    
    flow_ = 
      (traceProgramPred "select" "p_elementFound" p__ (if' p__
        (traceProgramNextOp "select" "o_found" (select_o_found data_))
        (traceProgramPred "select" "p_checkElement" p_1_ (if' p_1_
          (traceProgramNextOp "select" "o_searchLeft" (select_o_searchLeft data_))
          (traceProgramNextOp "select" "o_searchRight" (select_o_searchRight data_)) 
        )) 
      ))

--SHOW
instance Show Data_select where  
  show (Data_select end i list pivot res start end_iD_ i_iD_ list_iD_ pivot_iD_ res_iD_ start_iD_) = 
    (show [
      (if' end_iD_ (prepend 'd' (show end)) "u"),
      (if' i_iD_ (prepend 'd' (show i)) "u"),
      (if' list_iD_ (prepend 'd' (show list)) "u"),
      (if' pivot_iD_ (prepend 'd' (show pivot)) "u"),
      (if' res_iD_ (prepend 'd' (show res)) "u"),
      (if' start_iD_ (prepend 'd' (show start)) "u")
    ])

--END OF PROGRAM select

--START OF PROGRAM partition

data Data_partition  = Data_partition 
  Int --counter
  Int --cur
  Int --end
  [Int] --list
  Int --pivot
  Int --start
  Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
partition_list' :: [Int] -> Int -> Int -> Int -> [Int]
partition_pivot' :: [Int] -> Int -> Int -> Int -> Int

partition_o_init :: Data_partition -> Data_partition
partition_o_move :: Data_partition -> Data_partition
partition_o_skip :: Data_partition -> Data_partition
partition_o_swap :: Data_partition -> Data_partition
partition_o_swapTwice :: Data_partition -> Data_partition

partition_p_greaterPivot :: Data_partition -> Bool
partition_p_swap :: Data_partition -> Bool
partition_p_swap2 :: Data_partition -> Bool
partition_p_swapTwice :: Data_partition -> Bool
partition_p_validCount :: Data_partition -> Bool
partition_p_validList :: Data_partition -> Bool

--DEFINITIONS
partition_list' list start end pivot = (traceProgramCall "partition_list'" "partition" [("counter","Int"),("cur","Int"),("end","Int"),("list","[Int]"),("pivot","Int"),("start","Int")]  list')
  where
    (Data_partition _ _ _ list' _ _ _ _ _ _ _ _) = (partition_o_init (Data_partition undefined undefined end list pivot start False False True True True True))

partition_pivot' list start end pivot = (traceProgramCall "partition_pivot'" "partition" [("counter","Int"),("cur","Int"),("end","Int"),("list","[Int]"),("pivot","Int"),("start","Int")]  pivot')
  where
    (Data_partition _ _ _ _ pivot' _ _ _ _ _ _ _) = (partition_o_init (Data_partition undefined undefined end list pivot start False False True True True True))

partition_p_greaterPivot (Data_partition counter cur end list pivot start _ _ _ _ _ _) = pivot > cur

partition_p_swap (Data_partition counter cur end list pivot start _ _ _ _ _ _) = (get pivot list) <= (get cur list)

partition_p_swap2 (Data_partition counter cur end list pivot start _ _ _ _ _ _) = (get pivot list) > (get cur list)

partition_p_swapTwice (Data_partition counter cur end list pivot start _ _ _ _ _ _) = (get pivot list) <= (get (pivot + 1) list)

partition_p_validCount (Data_partition counter cur end list pivot start _ _ _ _ _ _) = counter > 0

partition_p_validList (Data_partition counter cur end list pivot start _ _ _ _ _ _) = (length list) > 1

partition_o_init (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = (traceProgramOp "partition" "o_init" data_ [True,True,False,False,False,False] flow_)
  where
    counter' = (end - start)
    cur' = start
    end' = end
    list' = list
    pivot' = pivot
    start' = start
    counter_iDn_ = True
    cur_iDn_ = True
    end_iDn_ = end_iD_
    list_iDn_ = list_iD_
    pivot_iDn_ = pivot_iD_
    start_iDn_ = start_iD_
    data_ = (Data_partition counter' cur' end' list' pivot' start' counter_iDn_ cur_iDn_ end_iDn_ list_iDn_ pivot_iDn_ start_iDn_)
    p__ = (partition_p_validList data_)    
    flow_ = 
      (traceProgramPred "partition" "p_validList" p__ (if' p__
        (traceProgramNextOp "partition" "o_move" (partition_o_move data_))
        (traceProgramHalt "partition" data_) 
      ))

partition_o_move (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = (traceProgramOp "partition" "o_move" data_ [False,False,False,False,False,False] flow_)
  where
    counter' = counter
    cur' = cur
    end' = end
    list' = list
    pivot' = pivot
    start' = start
    counter_iDn_ = counter_iD_
    cur_iDn_ = cur_iD_
    end_iDn_ = end_iD_
    list_iDn_ = list_iD_
    pivot_iDn_ = pivot_iD_
    start_iDn_ = start_iD_
    data_ = (Data_partition counter' cur' end' list' pivot' start' counter_iDn_ cur_iDn_ end_iDn_ list_iDn_ pivot_iDn_ start_iDn_)
    p__ = (partition_p_validCount data_)
    p_0_ = (partition_p_greaterPivot data_)
    p_0_0_ = (partition_p_swap data_)
    p_0_1_ = (partition_p_swap2 data_)
    p_0_1_0_ = (partition_p_swapTwice data_)    
    flow_ = 
      (traceProgramPred "partition" "p_validCount" p__ (if' p__
        (traceProgramPred "partition" "p_greaterPivot" p_0_ (if' p_0_
          (traceProgramPred "partition" "p_swap" p_0_0_ (if' p_0_0_
            (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          ))
          (traceProgramPred "partition" "p_swap2" p_0_1_ (if' p_0_1_
            (traceProgramPred "partition" "p_swapTwice" p_0_1_0_ (if' p_0_1_0_
              (traceProgramNextOp "partition" "o_swapTwice" (partition_o_swapTwice data_))
              (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_)) 
            ))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          )) 
        ))
        (traceProgramHalt "partition" data_) 
      ))

partition_o_skip (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = (traceProgramOp "partition" "o_skip" data_ [True,True,False,False,False,False] flow_)
  where
    counter' = counter - 1
    cur' = cur + 1
    end' = end
    list' = list
    pivot' = pivot
    start' = start
    counter_iDn_ = True
    cur_iDn_ = True
    end_iDn_ = end_iD_
    list_iDn_ = list_iD_
    pivot_iDn_ = pivot_iD_
    start_iDn_ = start_iD_
    data_ = (Data_partition counter' cur' end' list' pivot' start' counter_iDn_ cur_iDn_ end_iDn_ list_iDn_ pivot_iDn_ start_iDn_)
    p__ = (partition_p_validCount data_)
    p_0_ = (partition_p_greaterPivot data_)
    p_0_0_ = (partition_p_swap data_)
    p_0_1_ = (partition_p_swap2 data_)
    p_0_1_0_ = (partition_p_swapTwice data_)    
    flow_ = 
      (traceProgramPred "partition" "p_validCount" p__ (if' p__
        (traceProgramPred "partition" "p_greaterPivot" p_0_ (if' p_0_
          (traceProgramPred "partition" "p_swap" p_0_0_ (if' p_0_0_
            (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          ))
          (traceProgramPred "partition" "p_swap2" p_0_1_ (if' p_0_1_
            (traceProgramPred "partition" "p_swapTwice" p_0_1_0_ (if' p_0_1_0_
              (traceProgramNextOp "partition" "o_swapTwice" (partition_o_swapTwice data_))
              (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_)) 
            ))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          )) 
        ))
        (traceProgramHalt "partition" data_) 
      ))

partition_o_swap (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = (traceProgramOp "partition" "o_swap" data_ [True,True,False,True,True,False] flow_)
  where
    counter' = counter - 1
    cur' = cur + 1
    list' = (swap cur pivot list)
    pivot' = cur
    end' = end
    start' = start
    counter_iDn_ = True
    cur_iDn_ = True
    list_iDn_ = True
    pivot_iDn_ = True
    end_iDn_ = end_iD_
    start_iDn_ = start_iD_
    data_ = (Data_partition counter' cur' end' list' pivot' start' counter_iDn_ cur_iDn_ end_iDn_ list_iDn_ pivot_iDn_ start_iDn_)
    p__ = (partition_p_validCount data_)
    p_0_ = (partition_p_greaterPivot data_)
    p_0_0_ = (partition_p_swap data_)
    p_0_1_ = (partition_p_swap2 data_)
    p_0_1_0_ = (partition_p_swapTwice data_)    
    flow_ = 
      (traceProgramPred "partition" "p_validCount" p__ (if' p__
        (traceProgramPred "partition" "p_greaterPivot" p_0_ (if' p_0_
          (traceProgramPred "partition" "p_swap" p_0_0_ (if' p_0_0_
            (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          ))
          (traceProgramPred "partition" "p_swap2" p_0_1_ (if' p_0_1_
            (traceProgramPred "partition" "p_swapTwice" p_0_1_0_ (if' p_0_1_0_
              (traceProgramNextOp "partition" "o_swapTwice" (partition_o_swapTwice data_))
              (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_)) 
            ))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          )) 
        ))
        (traceProgramHalt "partition" data_) 
      ))

partition_o_swapTwice (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = (traceProgramOp "partition" "o_swapTwice" data_ [True,True,False,True,True,False] flow_)
  where
    counter' = counter-1
    cur' = cur + 1
    list' = (swap cur (pivot + 1) (swap cur pivot list))
    pivot' = pivot + 1
    end' = end
    start' = start
    counter_iDn_ = True
    cur_iDn_ = True
    list_iDn_ = True
    pivot_iDn_ = True
    end_iDn_ = end_iD_
    start_iDn_ = start_iD_
    data_ = (Data_partition counter' cur' end' list' pivot' start' counter_iDn_ cur_iDn_ end_iDn_ list_iDn_ pivot_iDn_ start_iDn_)
    p__ = (partition_p_validCount data_)
    p_0_ = (partition_p_greaterPivot data_)
    p_0_0_ = (partition_p_swap data_)
    p_0_1_ = (partition_p_swap2 data_)
    p_0_1_0_ = (partition_p_swapTwice data_)    
    flow_ = 
      (traceProgramPred "partition" "p_validCount" p__ (if' p__
        (traceProgramPred "partition" "p_greaterPivot" p_0_ (if' p_0_
          (traceProgramPred "partition" "p_swap" p_0_0_ (if' p_0_0_
            (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          ))
          (traceProgramPred "partition" "p_swap2" p_0_1_ (if' p_0_1_
            (traceProgramPred "partition" "p_swapTwice" p_0_1_0_ (if' p_0_1_0_
              (traceProgramNextOp "partition" "o_swapTwice" (partition_o_swapTwice data_))
              (traceProgramNextOp "partition" "o_swap" (partition_o_swap data_)) 
            ))
            (traceProgramNextOp "partition" "o_skip" (partition_o_skip data_)) 
          )) 
        ))
        (traceProgramHalt "partition" data_) 
      ))

--SHOW
instance Show Data_partition where  
  show (Data_partition counter cur end list pivot start counter_iD_ cur_iD_ end_iD_ list_iD_ pivot_iD_ start_iD_) = 
    (show [
      (if' counter_iD_ (prepend 'd' (show counter)) "u"),
      (if' cur_iD_ (prepend 'd' (show cur)) "u"),
      (if' end_iD_ (prepend 'd' (show end)) "u"),
      (if' list_iD_ (prepend 'd' (show list)) "u"),
      (if' pivot_iD_ (prepend 'd' (show pivot)) "u"),
      (if' start_iD_ (prepend 'd' (show start)) "u")
    ])

--END OF PROGRAM partition
