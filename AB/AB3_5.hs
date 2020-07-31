module AB3_5 where

import Prelude (putStrLn,return)
import SPLib.Basic 
  (Show(show), Int, Bool(True,False), undefined, if', mod, (+), (==),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List (length, prepend, first, remove)
import Control.DeepSeq (deepseq)


josephus :: Int -> Int
josephus = josephus'

main = do
  let tests = [josephus 6, josephus 16]  
  deepseq tests (putStrLn "End")

-- *** DO NOT MODIFY ABOVE CODE ***

--START OF PROGRAM josephus

data Data_josephus  = Data_josephus 
  [Int] --circle
  Int --cur
  Int --last
  Int --n
  Bool Bool Bool Bool
  
--SIGNATURES
josephus' :: Int -> Int

josephus_o_init :: Data_josephus -> Data_josephus
josephus_o_last :: Data_josephus -> Data_josephus
josephus_o_remove :: Data_josephus -> Data_josephus

josephus_p_last :: Data_josephus -> Bool

--DEFINITIONS
josephus' n = (traceProgramCall "josephus'" "josephus" [("circle","[Int]"),("cur","Int"),("last","Int"),("n","Int")]  last')
  where
    (Data_josephus _ _ last' _ _ _ _ _) = (josephus_o_init (Data_josephus undefined undefined undefined n False False False True))

josephus_p_last (Data_josephus circle cur last n _ _ _ _) = (length circle) == 1

josephus_o_init (Data_josephus circle cur last n circle_iD_ cur_iD_ last_iD_ n_iD_) = (traceProgramOp "josephus" "o_init" data_ [True,True,False,False] flow_)
  where
    circle' = [1..n]
    cur' = 0
    last' = last
    n' = n
    circle_iDn_ = True
    cur_iDn_ = True
    last_iDn_ = last_iD_
    n_iDn_ = n_iD_
    data_ = (Data_josephus circle' cur' last' n' circle_iDn_ cur_iDn_ last_iDn_ n_iDn_)
    p__ = (josephus_p_last data_)    
    flow_ = 
      (traceProgramPred "josephus" "p_last" p__ (if' p__
        (traceProgramNextOp "josephus" "o_last" (josephus_o_last data_))
        (traceProgramNextOp "josephus" "o_remove" (josephus_o_remove data_)) 
      ))

josephus_o_last (Data_josephus circle cur last n circle_iD_ cur_iD_ last_iD_ n_iD_) = (traceProgramOp "josephus" "o_last" data_ [False,False,True,False] flow_)
  where
    last' = (first circle)
    circle' = circle
    cur' = cur
    n' = n
    last_iDn_ = True
    circle_iDn_ = circle_iD_
    cur_iDn_ = cur_iD_
    n_iDn_ = n_iD_
    data_ = (Data_josephus circle' cur' last' n' circle_iDn_ cur_iDn_ last_iDn_ n_iDn_)
    
    flow_ = 
      (traceProgramHalt "josephus" data_)

josephus_o_remove (Data_josephus circle cur last n circle_iD_ cur_iD_ last_iD_ n_iD_) = (traceProgramOp "josephus" "o_remove" data_ [True,True,False,False] flow_)
  where
    circle' = if' ((length circle) == (cur + 1)) (remove 0 circle) (remove (cur + 1) circle)
    cur' = if' ((length circle) == (cur + 2)) 0 (if' ((length circle) == (cur + 1)) 0 (cur + 1))
    last' = last
    n' = n
    circle_iDn_ = True
    cur_iDn_ = True
    last_iDn_ = last_iD_
    n_iDn_ = n_iD_
    data_ = (Data_josephus circle' cur' last' n' circle_iDn_ cur_iDn_ last_iDn_ n_iDn_)
    p__ = (josephus_p_last data_)    
    flow_ = 
      (traceProgramPred "josephus" "p_last" p__ (if' p__
        (traceProgramNextOp "josephus" "o_last" (josephus_o_last data_))
        (traceProgramNextOp "josephus" "o_remove" (josephus_o_remove data_)) 
      ))

--SHOW
instance Show Data_josephus where  
  show (Data_josephus circle cur last n circle_iD_ cur_iD_ last_iD_ n_iD_) = 
    (show [
      (if' circle_iD_ (prepend 'd' (show circle)) "u"),
      (if' cur_iD_ (prepend 'd' (show cur)) "u"),
      (if' last_iD_ (prepend 'd' (show last)) "u"),
      (if' n_iD_ (prepend 'd' (show n)) "u")
    ])

--END OF PROGRAM josephus
