module AB3_3 where

import Prelude (putStrLn,return)
import SPLib.Basic 
  (Show(show), Int, Bool(True,False), undefined, if', not, mod, div, (-), (+), (==), (||), (&&),
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List (prepend)
import Control.DeepSeq (deepseq)

egyptMult :: Int -> Int -> Int
egyptMult = egyptMult'

main = do
  let tests = [egyptMult 5 16, egyptMult 7 11, egyptMult 0 4, egyptMult 4 0, egyptMult 3 1, egyptMult 3 25]  
  deepseq tests (putStrLn "End")

-- *** DO NOT MODIFY ABOVE CODE ***

--START OF PROGRAM egyptMult

data Data_egyptMult  = Data_egyptMult 
  Int --a
  Int --b
  Int --c
  Int --i
  Int --r
  Bool Bool Bool Bool Bool
  
--SIGNATURES
egyptMult' :: Int -> Int -> Int

egyptMult_o_addEven :: Data_egyptMult -> Data_egyptMult
egyptMult_o_addOdd :: Data_egyptMult -> Data_egyptMult
egyptMult_o_init :: Data_egyptMult -> Data_egyptMult
egyptMult_o_skipEven :: Data_egyptMult -> Data_egyptMult
egyptMult_o_skipOdd :: Data_egyptMult -> Data_egyptMult

egyptMult_p_addEven :: Data_egyptMult -> Bool
egyptMult_p_addOdd :: Data_egyptMult -> Bool
egyptMult_p_end :: Data_egyptMult -> Bool
egyptMult_p_even :: Data_egyptMult -> Bool

--DEFINITIONS
egyptMult' a b = (traceProgramCall "egyptMult'" "egyptMult" [("a","Int"),("b","Int"),("c","Int"),("i","Int"),("r","Int")]  r')
  where
    (Data_egyptMult _ _ _ _ r' _ _ _ _ _) = (egyptMult_o_init (Data_egyptMult a b undefined undefined undefined True True False False False))

egyptMult_p_addEven (Data_egyptMult a b c i r _ _ _ _ _) = (mod (div i 2) 2) == 1

egyptMult_p_addOdd (Data_egyptMult a b c i r _ _ _ _ _) = (mod (div i 2) 2) == 1

egyptMult_p_end (Data_egyptMult a b c i r _ _ _ _ _) = (i == 1) || (i == 0)

egyptMult_p_even (Data_egyptMult a b c i r _ _ _ _ _) = (mod i 2) == 0

egyptMult_o_addEven (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = (traceProgramOp "egyptMult" "o_addEven" data_ [False,False,True,True,True] flow_)
  where
    c' = (c + c)
    i' = (div i 2)
    r' = (r + c + c)
    a' = a
    b' = b
    c_iDn_ = True
    i_iDn_ = True
    r_iDn_ = True
    a_iDn_ = a_iD_
    b_iDn_ = b_iD_
    data_ = (Data_egyptMult a' b' c' i' r' a_iDn_ b_iDn_ c_iDn_ i_iDn_ r_iDn_)
    p__ = (egyptMult_p_end data_)
    p_1_ = (egyptMult_p_even data_)
    p_1_0_ = (egyptMult_p_addEven data_)
    p_1_1_ = (egyptMult_p_addOdd data_)    
    flow_ = 
      (traceProgramPred "egyptMult" "p_end" p__ (if' p__
        (traceProgramHalt "egyptMult" data_)
        (traceProgramPred "egyptMult" "p_even" p_1_ (if' p_1_
          (traceProgramPred "egyptMult" "p_addEven" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "egyptMult" "o_addEven" (egyptMult_o_addEven data_))
            (traceProgramNextOp "egyptMult" "o_skipEven" (egyptMult_o_skipEven data_)) 
          ))
          (traceProgramPred "egyptMult" "p_addOdd" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "egyptMult" "o_addOdd" (egyptMult_o_addOdd data_))
            (traceProgramNextOp "egyptMult" "o_skipOdd" (egyptMult_o_skipOdd data_)) 
          )) 
        )) 
      ))

egyptMult_o_addOdd (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = (traceProgramOp "egyptMult" "o_addOdd" data_ [False,False,True,True,True] flow_)
  where
    c' = (c + c)
    i' = (div i 2)
    r' = (r + c + c)
    a' = a
    b' = b
    c_iDn_ = True
    i_iDn_ = True
    r_iDn_ = True
    a_iDn_ = a_iD_
    b_iDn_ = b_iD_
    data_ = (Data_egyptMult a' b' c' i' r' a_iDn_ b_iDn_ c_iDn_ i_iDn_ r_iDn_)
    p__ = (egyptMult_p_end data_)
    p_1_ = (egyptMult_p_even data_)
    p_1_0_ = (egyptMult_p_addEven data_)
    p_1_1_ = (egyptMult_p_addOdd data_)    
    flow_ = 
      (traceProgramPred "egyptMult" "p_end" p__ (if' p__
        (traceProgramHalt "egyptMult" data_)
        (traceProgramPred "egyptMult" "p_even" p_1_ (if' p_1_
          (traceProgramPred "egyptMult" "p_addEven" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "egyptMult" "o_addEven" (egyptMult_o_addEven data_))
            (traceProgramNextOp "egyptMult" "o_skipEven" (egyptMult_o_skipEven data_)) 
          ))
          (traceProgramPred "egyptMult" "p_addOdd" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "egyptMult" "o_addOdd" (egyptMult_o_addOdd data_))
            (traceProgramNextOp "egyptMult" "o_skipOdd" (egyptMult_o_skipOdd data_)) 
          )) 
        )) 
      ))

egyptMult_o_init (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = (traceProgramOp "egyptMult" "o_init" data_ [False,False,True,True,True] flow_)
  where
    c' = a
    i' = b
    r' = if' ((mod b 2) == 0) 0 a
    a' = a
    b' = b
    c_iDn_ = True
    i_iDn_ = True
    r_iDn_ = True
    a_iDn_ = a_iD_
    b_iDn_ = b_iD_
    data_ = (Data_egyptMult a' b' c' i' r' a_iDn_ b_iDn_ c_iDn_ i_iDn_ r_iDn_)
    p__ = (egyptMult_p_end data_)
    p_1_ = (egyptMult_p_even data_)
    p_1_0_ = (egyptMult_p_addEven data_)
    p_1_1_ = (egyptMult_p_addOdd data_)    
    flow_ = 
      (traceProgramPred "egyptMult" "p_end" p__ (if' p__
        (traceProgramHalt "egyptMult" data_)
        (traceProgramPred "egyptMult" "p_even" p_1_ (if' p_1_
          (traceProgramPred "egyptMult" "p_addEven" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "egyptMult" "o_addEven" (egyptMult_o_addEven data_))
            (traceProgramNextOp "egyptMult" "o_skipEven" (egyptMult_o_skipEven data_)) 
          ))
          (traceProgramPred "egyptMult" "p_addOdd" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "egyptMult" "o_addOdd" (egyptMult_o_addOdd data_))
            (traceProgramNextOp "egyptMult" "o_skipOdd" (egyptMult_o_skipOdd data_)) 
          )) 
        )) 
      ))

egyptMult_o_skipEven (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = (traceProgramOp "egyptMult" "o_skipEven" data_ [False,False,True,True,False] flow_)
  where
    c' = (c + c)
    i' = (div i 2)
    a' = a
    b' = b
    r' = r
    c_iDn_ = True
    i_iDn_ = True
    a_iDn_ = a_iD_
    b_iDn_ = b_iD_
    r_iDn_ = r_iD_
    data_ = (Data_egyptMult a' b' c' i' r' a_iDn_ b_iDn_ c_iDn_ i_iDn_ r_iDn_)
    p__ = (egyptMult_p_end data_)
    p_1_ = (egyptMult_p_even data_)
    p_1_0_ = (egyptMult_p_addEven data_)
    p_1_1_ = (egyptMult_p_addOdd data_)    
    flow_ = 
      (traceProgramPred "egyptMult" "p_end" p__ (if' p__
        (traceProgramHalt "egyptMult" data_)
        (traceProgramPred "egyptMult" "p_even" p_1_ (if' p_1_
          (traceProgramPred "egyptMult" "p_addEven" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "egyptMult" "o_addEven" (egyptMult_o_addEven data_))
            (traceProgramNextOp "egyptMult" "o_skipEven" (egyptMult_o_skipEven data_)) 
          ))
          (traceProgramPred "egyptMult" "p_addOdd" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "egyptMult" "o_addOdd" (egyptMult_o_addOdd data_))
            (traceProgramNextOp "egyptMult" "o_skipOdd" (egyptMult_o_skipOdd data_)) 
          )) 
        )) 
      ))

egyptMult_o_skipOdd (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = (traceProgramOp "egyptMult" "o_skipOdd" data_ [False,False,True,True,False] flow_)
  where
    c' = (c + c)
    i' = (div i 2)
    a' = a
    b' = b
    r' = r
    c_iDn_ = True
    i_iDn_ = True
    a_iDn_ = a_iD_
    b_iDn_ = b_iD_
    r_iDn_ = r_iD_
    data_ = (Data_egyptMult a' b' c' i' r' a_iDn_ b_iDn_ c_iDn_ i_iDn_ r_iDn_)
    p__ = (egyptMult_p_end data_)
    p_1_ = (egyptMult_p_even data_)
    p_1_0_ = (egyptMult_p_addEven data_)
    p_1_1_ = (egyptMult_p_addOdd data_)    
    flow_ = 
      (traceProgramPred "egyptMult" "p_end" p__ (if' p__
        (traceProgramHalt "egyptMult" data_)
        (traceProgramPred "egyptMult" "p_even" p_1_ (if' p_1_
          (traceProgramPred "egyptMult" "p_addEven" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "egyptMult" "o_addEven" (egyptMult_o_addEven data_))
            (traceProgramNextOp "egyptMult" "o_skipEven" (egyptMult_o_skipEven data_)) 
          ))
          (traceProgramPred "egyptMult" "p_addOdd" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "egyptMult" "o_addOdd" (egyptMult_o_addOdd data_))
            (traceProgramNextOp "egyptMult" "o_skipOdd" (egyptMult_o_skipOdd data_)) 
          )) 
        )) 
      ))

--SHOW
instance Show Data_egyptMult where  
  show (Data_egyptMult a b c i r a_iD_ b_iD_ c_iD_ i_iD_ r_iD_) = 
    (show [
      (if' a_iD_ (prepend 'd' (show a)) "u"),
      (if' b_iD_ (prepend 'd' (show b)) "u"),
      (if' c_iD_ (prepend 'd' (show c)) "u"),
      (if' i_iD_ (prepend 'd' (show i)) "u"),
      (if' r_iD_ (prepend 'd' (show r)) "u")
    ])

--END OF PROGRAM egyptMult
