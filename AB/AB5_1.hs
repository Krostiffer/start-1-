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

--start of program select
select' = undefined
--end of program select <- Edit both start and end to UPPERCASE like below. Doesn't compile otherwise.

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

partition_p_swapTwice (Data_partition counter cur end list pivot start _ _ _ _ _ _) = (get pivot list) <= (get (pivot+1) list)

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
    counter' = counter-1
    cur' = cur+1
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
    counter' = counter-1
    cur' = cur+1
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
    cur' = cur+1
    list' = (swap cur (pivot+1) (swap cur pivot list))
    pivot' = pivot+1
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
