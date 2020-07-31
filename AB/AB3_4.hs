module AB3_4 where

import Prelude (putStrLn,return)
import Types (CSMInstance, createCSM, coa, cob, lena, lenb, ssc)
import SPLib.Basic 
  (Show(show), Int, Bool(True,False), String, undefined, if', not, (<), (+), (-), (==), (<=), (<), (&&), (||),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List (prepend, map)
import Control.DeepSeq (deepseq)


csm :: [String] -> [String] -> Bool
csm a b = csm' (createCSM a b)

main = do
  let tests = [createCSM ["abab","ab","ab","ab"] ["ab","aba","bab","ab"],createCSM ["abab","ab","ab","ab"] ["ab","aba","bb","ab"],createCSM [] [""], createCSM ["",""] [], createCSM ["a"] ["b"]]   --csm test
  deepseq (map csm' tests) (putStrLn "End")

  
-- *** DO NOT MODIFY ABOVE CODE ***

--START OF PROGRAM csmAlg

data Data_csmAlg  = Data_csmAlg 
  Int --ca
  Int --cb
  Int --l
  Int --oa
  Int --ob
  Bool --r
  CSMInstance --x
  Bool Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
csm' :: CSMInstance -> Bool

csmAlg_o_alen :: Data_csmAlg -> Data_csmAlg
csmAlg_o_anbn :: Data_csmAlg -> Data_csmAlg
csmAlg_o_anbs :: Data_csmAlg -> Data_csmAlg
csmAlg_o_asbn :: Data_csmAlg -> Data_csmAlg
csmAlg_o_blen :: Data_csmAlg -> Data_csmAlg
csmAlg_o_init :: Data_csmAlg -> Data_csmAlg
csmAlg_o_no :: Data_csmAlg -> Data_csmAlg
csmAlg_o_nop :: Data_csmAlg -> Data_csmAlg
csmAlg_o_yes :: Data_csmAlg -> Data_csmAlg

csmAlg_p_aleq :: Data_csmAlg -> Bool
csmAlg_p_empty :: Data_csmAlg -> Bool
csmAlg_p_eob :: Data_csmAlg -> Bool
csmAlg_p_eqlen :: Data_csmAlg -> Bool
csmAlg_p_fc :: Data_csmAlg -> Bool
csmAlg_p_ss :: Data_csmAlg -> Bool

--DEFINITIONS
csm' x = (traceProgramCall "csm'" "csmAlg" [("ca","Int"),("cb","Int"),("l","Int"),("oa","Int"),("ob","Int"),("r","Bool"),("x","CSMInstance")]  r')
  where
    (Data_csmAlg _ _ _ _ _ r' _ _ _ _ _ _ _ _) = (csmAlg_o_init (Data_csmAlg undefined undefined undefined undefined undefined undefined x False False False False False False True))

csmAlg_p_aleq (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = (((lena x ca) - oa) <= ((lenb x cb) - ob))

csmAlg_p_empty (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = (strlena x (coa x) -1) == 0

csmAlg_p_eob (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = ((lenb x cb) == (l + ob))

csmAlg_p_eqlen (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = (strlena x (coa x) -1) == (strlenb x (cob x) -1)

csmAlg_p_fc (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = ((ca == ((coa x) - 1)) && (cb == ((cob x) - 1)))

csmAlg_p_ss (Data_csmAlg ca cb l oa ob r x _ _ _ _ _ _ _) = (ssc x ca cb oa ob l)

csmAlg_o_alen (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_alen" data_ [False,False,True,False,False,False,False] flow_)
  where
    l' = ((lena x ca) - oa)
    ca' = ca
    cb' = cb
    oa' = oa
    ob' = ob
    r' = r
    x' = x
    l_iDn_ = True
    ca_iDn_ = ca_iD_
    cb_iDn_ = cb_iD_
    oa_iDn_ = oa_iD_
    ob_iDn_ = ob_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    p__ = (csmAlg_p_ss data_)
    p_0_ = (csmAlg_p_fc data_)
    p_0_1_ = (csmAlg_p_eob data_)    
    flow_ = 
      (traceProgramPred "csmAlg" "p_ss" p__ (if' p__
        (traceProgramPred "csmAlg" "p_fc" p_0_ (if' p_0_
          (traceProgramNextOp "csmAlg" "o_yes" (csmAlg_o_yes data_))
          (traceProgramPred "csmAlg" "p_eob" p_0_1_ (if' p_0_1_
            (traceProgramNextOp "csmAlg" "o_anbn" (csmAlg_o_anbn data_))
            (traceProgramNextOp "csmAlg" "o_anbs" (csmAlg_o_anbs data_)) 
          )) 
        ))
        (traceProgramNextOp "csmAlg" "o_no" (csmAlg_o_no data_)) 
      ))

csmAlg_o_anbn (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_anbn" data_ [True,True,False,True,True,False,False] flow_)
  where
    ca' = (ca + 1)
    cb' = (cb + 1)
    oa' = 0
    ob' = 0
    l' = l
    r' = r
    x' = x
    ca_iDn_ = True
    cb_iDn_ = True
    oa_iDn_ = True
    ob_iDn_ = True
    l_iDn_ = l_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    
    flow_ = 
      (traceProgramNextOp "csmAlg" "o_nop" (csmAlg_o_nop data_))

csmAlg_o_anbs (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_anbs" data_ [True,False,False,True,True,False,False] flow_)
  where
    ca' = (ca + 1)
    oa' = l
    ob' = 1
    cb' = cb
    l' = l
    r' = r
    x' = x
    ca_iDn_ = True
    oa_iDn_ = True
    ob_iDn_ = True
    cb_iDn_ = cb_iD_
    l_iDn_ = l_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    
    flow_ = 
      (traceProgramNextOp "csmAlg" "o_nop" (csmAlg_o_nop data_))

csmAlg_o_asbn (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_asbn" data_ [False,True,False,True,True,False,False] flow_)
  where
    cb' = (cb + 1)
    oa' = 0
    ob' = l
    ca' = ca
    l' = l
    r' = r
    x' = x
    cb_iDn_ = True
    oa_iDn_ = True
    ob_iDn_ = True
    ca_iDn_ = ca_iD_
    l_iDn_ = l_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    
    flow_ = 
      (traceProgramNextOp "csmAlg" "o_nop" (csmAlg_o_nop data_))

csmAlg_o_blen (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_blen" data_ [False,False,True,False,False,False,False] flow_)
  where
    l' = ((lenb x cb) - ob)
    ca' = ca
    cb' = cb
    oa' = oa
    ob' = ob
    r' = r
    x' = x
    l_iDn_ = True
    ca_iDn_ = ca_iD_
    cb_iDn_ = cb_iD_
    oa_iDn_ = oa_iD_
    ob_iDn_ = ob_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    p__ = (csmAlg_p_ss data_)    
    flow_ = 
      (traceProgramPred "csmAlg" "p_ss" p__ (if' p__
        (traceProgramNextOp "csmAlg" "o_asbn" (csmAlg_o_asbn data_))
        (traceProgramNextOp "csmAlg" "o_no" (csmAlg_o_no data_)) 
      ))

csmAlg_o_init (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_init" data_ [True,True,False,True,True,False,False] flow_)
  where
    ca' = 0
    cb' = 0
    oa' = 0
    ob' = 0
    l' = l
    r' = r
    x' = x
    ca_iDn_ = True
    cb_iDn_ = True
    oa_iDn_ = True
    ob_iDn_ = True
    l_iDn_ = l_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    p__ = (csmAlg_p_eqlen data_)
    p_0_ = (csmAlg_p_empty data_)    
    flow_ = 
      (traceProgramPred "csmAlg" "p_eqlen" p__ (if' p__
        (traceProgramPred "csmAlg" "p_empty" p_0_ (if' p_0_
          (traceProgramNextOp "csmAlg" "o_yes" (csmAlg_o_yes data_))
          (traceProgramNextOp "csmAlg" "o_nop" (csmAlg_o_nop data_)) 
        ))
        (traceProgramNextOp "csmAlg" "o_no" (csmAlg_o_no data_)) 
      ))

csmAlg_o_no (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_no" data_ [False,False,False,False,False,True,False] flow_)
  where
    r' = False
    ca' = ca
    cb' = cb
    l' = l
    oa' = oa
    ob' = ob
    x' = x
    r_iDn_ = True
    ca_iDn_ = ca_iD_
    cb_iDn_ = cb_iD_
    l_iDn_ = l_iD_
    oa_iDn_ = oa_iD_
    ob_iDn_ = ob_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    
    flow_ = 
      (traceProgramHalt "csmAlg" data_)

csmAlg_o_nop (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_nop" data_ [False,False,False,False,False,False,False] flow_)
  where
    ca' = ca
    cb' = cb
    l' = l
    oa' = oa
    ob' = ob
    r' = r
    x' = x
    ca_iDn_ = ca_iD_
    cb_iDn_ = cb_iD_
    l_iDn_ = l_iD_
    oa_iDn_ = oa_iD_
    ob_iDn_ = ob_iD_
    r_iDn_ = r_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    p__ = (csmAlg_p_aleq data_)    
    flow_ = 
      (traceProgramPred "csmAlg" "p_aleq" p__ (if' p__
        (traceProgramNextOp "csmAlg" "o_alen" (csmAlg_o_alen data_))
        (traceProgramNextOp "csmAlg" "o_blen" (csmAlg_o_blen data_)) 
      ))

csmAlg_o_yes (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = (traceProgramOp "csmAlg" "o_yes" data_ [False,False,False,False,False,True,False] flow_)
  where
    r' = True
    ca' = ca
    cb' = cb
    l' = l
    oa' = oa
    ob' = ob
    x' = x
    r_iDn_ = True
    ca_iDn_ = ca_iD_
    cb_iDn_ = cb_iD_
    l_iDn_ = l_iD_
    oa_iDn_ = oa_iD_
    ob_iDn_ = ob_iD_
    x_iDn_ = x_iD_
    data_ = (Data_csmAlg ca' cb' l' oa' ob' r' x' ca_iDn_ cb_iDn_ l_iDn_ oa_iDn_ ob_iDn_ r_iDn_ x_iDn_)
    
    flow_ = 
      (traceProgramHalt "csmAlg" data_)

--SHOW
instance Show Data_csmAlg where  
  show (Data_csmAlg ca cb l oa ob r x ca_iD_ cb_iD_ l_iD_ oa_iD_ ob_iD_ r_iD_ x_iD_) = 
    (show [
      (if' ca_iD_ (prepend 'd' (show ca)) "u"),
      (if' cb_iD_ (prepend 'd' (show cb)) "u"),
      (if' l_iD_ (prepend 'd' (show l)) "u"),
      (if' oa_iD_ (prepend 'd' (show oa)) "u"),
      (if' ob_iD_ (prepend 'd' (show ob)) "u"),
      (if' r_iD_ (prepend 'd' (show r)) "u"),
      (if' x_iD_ (prepend 'd' (show x)) "u")
    ])

--END OF PROGRAM csmAlg

strlena :: CSMInstance -> Int -> Int
strlena a i = if' (i < 0) 0 (if' (i <= 0) (lena a i) ((lena a i) + (strlena a (i-1))))
strlenb :: CSMInstance -> Int -> Int
strlenb a i = if' (i < 0) 0 (if' (i == 0) (lenb a i) ((lenb a i) + (strlenb a (i-1))))