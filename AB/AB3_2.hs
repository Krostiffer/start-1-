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
bubbleSort' = undefined
--END OF PROGRAM bubbleSort
