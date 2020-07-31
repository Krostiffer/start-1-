module AB3_2 where

import Prelude (putStrLn,return)
import AB2_1 (swap)
import SPLib.Basic 
  (Show(show), Int, Bool(True,False), trace, undefined, if', not, (+), (==), (<), (&&),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List (get, length, prepend)
import Control.DeepSeq (deepseq)

bubbleSort :: [Int] -> [Int] 
bubbleSort = bubbleSort'

main = do
  let tests = [bubbleSort [1], bubbleSort [1,2,3,4], bubbleSort [4,3,2,1], bubbleSort [1,5,4,6,2,3,1]]  
  deepseq tests (putStrLn "End")

-- *** DO NOT MODIFY ABOVE CODE ***

--START OF PROGRAM bubbleSort

data Data_bubbleSort  = Data_bubbleSort 
  Int --cur
  [Int] --list
  Int --passes
  Bool --sorted
  Bool Bool Bool Bool
  
--SIGNATURES
bubbleSort' :: [Int] -> [Int]

bubbleSort_o_init :: Data_bubbleSort -> Data_bubbleSort
bubbleSort_o_move :: Data_bubbleSort -> Data_bubbleSort
bubbleSort_o_next :: Data_bubbleSort -> Data_bubbleSort
bubbleSort_o_swap :: Data_bubbleSort -> Data_bubbleSort

bubbleSort_p_endOfIteration :: Data_bubbleSort -> Bool
bubbleSort_p_lt2 :: Data_bubbleSort -> Bool
bubbleSort_p_nextIteration :: Data_bubbleSort -> Bool
bubbleSort_p_swap :: Data_bubbleSort -> Bool

--DEFINITIONS
bubbleSort' list = (traceProgramCall "bubbleSort'" "bubbleSort" [("cur","Int"),("list","[Int]"),("passes","Int"),("sorted","Bool")]  list')
  where
    (Data_bubbleSort _ list' _ _ _ _ _ _) = (bubbleSort_o_init (Data_bubbleSort undefined list undefined undefined False True False False))

bubbleSort_p_endOfIteration (Data_bubbleSort cur list passes sorted _ _ _ _) = (length list) < (cur +2)

bubbleSort_p_lt2 (Data_bubbleSort cur list passes sorted _ _ _ _) = (length list) < 3

bubbleSort_p_nextIteration (Data_bubbleSort cur list passes sorted _ _ _ _) = (not sorted)

bubbleSort_p_swap (Data_bubbleSort cur list passes sorted _ _ _ _) = (get (cur +1) list) < (get cur list)

bubbleSort_o_init (Data_bubbleSort cur list passes sorted cur_iD_ list_iD_ passes_iD_ sorted_iD_) = (traceProgramOp "bubbleSort" "o_init" data_ [True,False,True,True] flow_)
  where
    cur' = 0
    passes' = 0
    sorted' = True
    list' = list
    cur_iDn_ = True
    passes_iDn_ = True
    sorted_iDn_ = True
    list_iDn_ = list_iD_
    data_ = (Data_bubbleSort cur' list' passes' sorted' cur_iDn_ list_iDn_ passes_iDn_ sorted_iDn_)
    p__ = (bubbleSort_p_lt2 data_)
    p_1_ = (bubbleSort_p_swap data_)    
    flow_ = 
      (traceProgramPred "bubbleSort" "p_lt2" p__ (if' p__
        (traceProgramHalt "bubbleSort" data_)
        (traceProgramPred "bubbleSort" "p_swap" p_1_ (if' p_1_
          (traceProgramNextOp "bubbleSort" "o_swap" (bubbleSort_o_swap data_))
          (traceProgramNextOp "bubbleSort" "o_move" (bubbleSort_o_move data_)) 
        )) 
      ))

bubbleSort_o_move (Data_bubbleSort cur list passes sorted cur_iD_ list_iD_ passes_iD_ sorted_iD_) = (traceProgramOp "bubbleSort" "o_move" data_ [True,False,False,False] flow_)
  where
    cur' = cur + 1
    list' = list
    passes' = passes
    sorted' = sorted
    cur_iDn_ = True
    list_iDn_ = list_iD_
    passes_iDn_ = passes_iD_
    sorted_iDn_ = sorted_iD_
    data_ = (Data_bubbleSort cur' list' passes' sorted' cur_iDn_ list_iDn_ passes_iDn_ sorted_iDn_)
    p__ = (bubbleSort_p_endOfIteration data_)
    p_0_ = (bubbleSort_p_nextIteration data_)
    p_1_ = (bubbleSort_p_swap data_)    
    flow_ = 
      (traceProgramPred "bubbleSort" "p_endOfIteration" p__ (if' p__
        (traceProgramPred "bubbleSort" "p_nextIteration" p_0_ (if' p_0_
          (traceProgramNextOp "bubbleSort" "o_next" (bubbleSort_o_next data_))
          (traceProgramHalt "bubbleSort" data_) 
        ))
        (traceProgramPred "bubbleSort" "p_swap" p_1_ (if' p_1_
          (traceProgramNextOp "bubbleSort" "o_swap" (bubbleSort_o_swap data_))
          (traceProgramNextOp "bubbleSort" "o_move" (bubbleSort_o_move data_)) 
        )) 
      ))

bubbleSort_o_next (Data_bubbleSort cur list passes sorted cur_iD_ list_iD_ passes_iD_ sorted_iD_) = (traceProgramOp "bubbleSort" "o_next" data_ [True,False,True,True] flow_)
  where
    cur' = 0
    passes' = passes + 1
    sorted' = True
    list' = list
    cur_iDn_ = True
    passes_iDn_ = True
    sorted_iDn_ = True
    list_iDn_ = list_iD_
    data_ = (Data_bubbleSort cur' list' passes' sorted' cur_iDn_ list_iDn_ passes_iDn_ sorted_iDn_)
    p__ = (bubbleSort_p_swap data_)    
    flow_ = 
      (traceProgramPred "bubbleSort" "p_swap" p__ (if' p__
        (traceProgramNextOp "bubbleSort" "o_swap" (bubbleSort_o_swap data_))
        (traceProgramNextOp "bubbleSort" "o_move" (bubbleSort_o_move data_)) 
      ))

bubbleSort_o_swap (Data_bubbleSort cur list passes sorted cur_iD_ list_iD_ passes_iD_ sorted_iD_) = (traceProgramOp "bubbleSort" "o_swap" data_ [True,True,False,True] flow_)
  where
    cur' = cur + 1
    list' = (swap cur (cur+1) list)
    sorted' = False
    passes' = passes
    cur_iDn_ = True
    list_iDn_ = True
    sorted_iDn_ = True
    passes_iDn_ = passes_iD_
    data_ = (Data_bubbleSort cur' list' passes' sorted' cur_iDn_ list_iDn_ passes_iDn_ sorted_iDn_)
    p__ = (bubbleSort_p_endOfIteration data_)
    p_0_ = (bubbleSort_p_nextIteration data_)
    p_1_ = (bubbleSort_p_swap data_)    
    flow_ = 
      (traceProgramPred "bubbleSort" "p_endOfIteration" p__ (if' p__
        (traceProgramPred "bubbleSort" "p_nextIteration" p_0_ (if' p_0_
          (traceProgramNextOp "bubbleSort" "o_next" (bubbleSort_o_next data_))
          (traceProgramHalt "bubbleSort" data_) 
        ))
        (traceProgramPred "bubbleSort" "p_swap" p_1_ (if' p_1_
          (traceProgramNextOp "bubbleSort" "o_swap" (bubbleSort_o_swap data_))
          (traceProgramNextOp "bubbleSort" "o_move" (bubbleSort_o_move data_)) 
        )) 
      ))

--SHOW
instance Show Data_bubbleSort where  
  show (Data_bubbleSort cur list passes sorted cur_iD_ list_iD_ passes_iD_ sorted_iD_) = 
    (show [
      (if' cur_iD_ (prepend 'd' (show cur)) "u"),
      (if' list_iD_ (prepend 'd' (show list)) "u"),
      (if' passes_iD_ (prepend 'd' (show passes)) "u"),
      (if' sorted_iD_ (prepend 'd' (show sorted)) "u")
    ])

--END OF PROGRAM bubbleSort
