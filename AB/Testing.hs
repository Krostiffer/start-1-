{-
  This file can be used as template for test files. 
  Add imports for the ABx_y modules you want to test. 
-}

module Testing where

import Prelude ()
import Types as T 
import SPLib.Basic 
import SPLib.List as L
import SPLib.Tree as T
import SPLib.Map  as M 

--example imports 
import AB1_3 (crossList)
import AB1_4 (toDigits,fromDigits,length)


test1 = toDigits 2 18

--name clashes can be resolved as follows
strLen = L.length "abc"     --length function from SPLib.List
digLen = AB1_4.length 2 8   --length function from AB1_4

--the example automata from AB4.html 
m1 = DFA "z0" ["z3"]
  (fromList [
    (("z0",'a'),"z1"),
    (("z0",'b'),"z3"),
    (("z1",'a'),"z2"),
    (("z1",'b'),"z0"),
    (("z2",'a'),"z3"),
    (("z2",'b'),"z1"),
    (("z3",'a'),"z0"),
    (("z3",'b'),"z2")
  ] ) 
  
m2 = NFA "z0" ["z2"]  
  (fromList [
    (("z0",'0'),["z0","z1"]),
    (("z0",'1'),["z0"]),
    (("z1",'0'),["z2"])
  ]) 

m3 = NFA "z0" ["z3"]  
  (fromList [
    (("z0",'0'),["z0","z1"]),
    (("z0",'1'),["z0"]),
    (("z1",'0'),["z2"]),
    (("z1",'1'),["z0","z2"]),
    (("z2",'0'),["z0"]),
    (("z2",'1'),["z3"])
  ]) 
