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
egyptMult' = undefined
--END OF PROGRAM egyptMult
