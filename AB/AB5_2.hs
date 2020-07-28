module AB5_2 where

import Prelude (Ordering(LT,EQ,GT)) --Ordering used in conjunction with sortBy 
import SPLib.Basic 
  (Show(show), Char, String, Int, Bool(True,False), undefined, if', (+), (*), (<), (==), (.), max, fst,
  trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt)
import SPLib.List as L 
  (isEmpty, length, prepend, append, removeFirst, removeLast, first, last, get, 
  map, reduce, filter, foldl, zip, concat, take, drop, findIndex, sort, sortBy)
import SPLib.Map as M (Map, get, set, empty, fromList, toList)
import SPLib.Tree as T (Tree(Tree),rootDegree,rootLabel,getLabel,degree)


type CodeAlphabet   = [Char]
type Probabilities  = Map Char (Int,Int) --(numerator, denominator)
type HuffmanTree    = (Tree [Char])

huffmanTree   :: CodeAlphabet -> Probabilities -> HuffmanTree
huffmanEncode :: HuffmanTree -> CodeAlphabet -> String -> String
huffmanDecode :: HuffmanTree -> CodeAlphabet -> String -> String

huffmanDecode = huffmanDecode'

-- *** DO NOT MODIFY ABOVE CODE ***

huffmanTree = undefined
huffmanEncode = undefined

--START OF PROGRAM decode
huffmanDecode' = undefined
--END OF PROGRAM decode