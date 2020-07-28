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
josephus' = undefined
--END OF PROGRAM josephus
