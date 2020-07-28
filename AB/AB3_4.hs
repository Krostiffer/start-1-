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
csm' :: CSMInstance -> Bool
csm' = undefined
--END OF PROGRAM csmAlg
