module SPLib.List 
  (isEmpty, length, get, getD, getP, first, firstD, firstP, SPLib.List.last, lastD, lastP, set, setD, setP, prepend, append, insert, insertD, insertP, concat, remove, removeD, removeP, removeFirst, removeFirstD, removeFirstP, removeLast, removeLastD, removeLastP, contains, removeDuplicates, L.sort, L.sortBy, reverse, take, drop, splitAt, zip, map, filter, reduce, reduceD, reduceP, foldl, foldr, SPLib.List.find, findD, findP, SPLib.List.findIndex, findIndexD, findIndexP, repeat, join, split, splitD, splitP, splitAtFirst, splitAtFirstD, splitAtFirstP
  ) where

import Prelude as P (elem, reverse, take, drop, zip, map, filter, foldl, foldr, null, length, head, last, concat, tail, init, splitAt, repeat, (!!)) 
import qualified Data.List as L (sort, sortBy, find, findIndex, intercalate, nub) 

import SPLib.Basic
import SPLib.PartialFunction

type CallLocation = String

--SIGNATURES

isEmpty   :: [a] -> Bool
--length  :: [a] -> Int 

get      :: Int -> [a] -> a  
getD     :: CallLocation -> Int -> [a] -> a  
getP     :: PartialFunction (Int, [a]) a   
first    :: [a] -> a 
firstD   :: CallLocation -> [a] -> a  
firstP   :: PartialFunction [a] a
--last   :: [a] -> a
lastD    :: CallLocation -> [a] -> a
lastP    :: PartialFunction [a] a 

set       :: a -> Int -> [a] -> [a]
setD      :: CallLocation -> a -> Int -> [a] -> [a]
setP      :: PartialFunction (a, Int, [a]) [a]
prepend   :: a -> [a] -> [a]
append    :: a -> [a] -> [a]
insert    :: a -> Int -> [a] -> [a]  -- (get i (insert x i list)) = x
insertD   :: CallLocation -> a -> Int -> [a] -> [a] 
insertP   :: PartialFunction (a, Int, [a]) [a]
--concat  :: [[a]] -> [a]

remove       :: Int -> [a] -> [a]        
removeD      :: CallLocation -> Int -> [a] -> [a]        
removeP      :: PartialFunction (Int, [a]) [a]        
removeFirst  :: [a] -> [a]
removeFirstD :: CallLocation -> [a] -> [a]
removeFirstP :: PartialFunction [a] [a]
removeLast   :: [a] -> [a]    
removeLastD  :: CallLocation -> [a] -> [a]    
removeLastP  :: PartialFunction [a] [a]    
        
contains  :: Eq a => a -> [a] -> Bool
removeDuplicates :: Eq a => [a] -> [a] 
--sort    :: Ord a => [a] -> [a]
--reverse :: [a] -> [a]    
--take    :: Int -> [a] -> [a]
--drop    :: Int -> [a] -> [a]
--splitAt :: Int -> [a] -> ([a],[a])
--zip     :: [a] -> [b] -> [(a,b)]   
    
--map     :: (a -> b) -> [a] -> [b]
--filter  :: (a -> Bool) -> [a] -> [a] 
reduce    :: (a -> a -> a) -> [a] -> a   
reduceD   :: CallLocation -> (a -> a -> a) -> [a] -> a   
reduceP   :: PartialFunction ((a -> a -> a),[a]) a
--foldl   :: (b -> a -> b) -> b -> [a] -> b
--foldr   :: (a -> b -> b) -> b -> [a] -> b

find       :: (a -> Bool) -> [a] -> a 
findD      :: CallLocation -> (a -> Bool) -> [a] -> a 
findP      :: PartialFunction ((a -> Bool),[a]) a
findIndex  :: (a -> Bool) -> [a] -> Int
findIndexD :: CallLocation -> (a -> Bool) -> [a] -> Int
findIndexP :: PartialFunction ((a -> Bool),[a]) Int

--repeat :: a -> [a]

join          :: [a] -> [[a]] -> [a]           
split         :: String -> String -> [String]
splitD        :: CallLocation -> String -> String -> [String]
splitP        :: PartialFunction (String,String) [String]
splitAtFirst  :: String -> String -> (String,String)
splitAtFirstD :: CallLocation -> String -> String -> (String,String)
splitAtFirstP :: PartialFunction (String,String) (String,String)

{-
split         :: [a] -> [a] -> [[a]]
splitP        :: PartialFunction ([a],[a]) [[a]]
splitAtFirst  :: [a] -> [a] -> ([a],[a])
splitAtFirstP :: PartialFunction ([a],[a]) ([a],[a])
-}


-- DEFINITIONS 

isEmpty = P.null
--length = P.length    
    
get  = unpack2 . apply $ getP
getD cl = unpack2 . (apply' ("SPLib.List.getD @ "++cl)) $ getP
getP = PartialFunction "SPLib.List.getP" f f_ood f_err
  where
    f (index,list) = list!!index
    f_ood (index,list) = P.concat [r1,r2]
      where
        r1 = if' (index < 0)              [(0,[(show index),(show (P.length list))])] []
        r2 = if' (index >= (P.length list)) [(1,[(show index),(show (P.length list))])] []
    f_err = 
      [
        ["negative index ","; list has "," elements"],
        ["index "," out of bounds; list has "," elements"]
      ]

first = apply firstP 
firstD cl = apply' ("SPLib.List.firstD @ "++cl) firstP
firstP = PartialFunction "SPLib.List.firstP" f f_ood f_err
  where
    f list = head list
    f_ood list = if' (isEmpty list) [(0,[])] []
    f_err = [["empty list"]]

last = apply lastP 
lastD cl = apply' ("SPLib.List.lastD @ "++cl) lastP
lastP = PartialFunction "SPLib.List.lastP" f f_ood f_err
  where
    f list = P.last list
    f_ood list = if' (isEmpty list) [(0,[])] []
    f_err = [["empty list"]]


set  = unpack3 . apply $ setP
setD cl = unpack3 . (apply' ("SPLib.List.setD @ "++cl)) $ setP
setP = PartialFunction "SPLib.List.setP" f f_ood f_err
  where
    f (val,index,list) = x++(prepend val (removeFirst y))
      where
        (x,y) = (P.splitAt index list)
    f_ood (_,index,list) = P.concat [r1,r2]
      where
        r1 = if' (index < 0)              [(0,[(show index),(show (P.length list))])] []
        r2 = if' (index >= (P.length list)) [(1,[(show index),(show (P.length list))])] []
    f_err = 
      [
        ["negative index ","; list has "," elements"],
        ["index "," out of bounds; list has "," elements"]
      ]
      
prepend val list = val:list
append  val list = list++[val]
insert  = unpack3 . apply $ insertP
insertD cl = unpack3 . (apply' ("SPLib.List.insertD @ "++cl)) $ insertP
insertP = PartialFunction "SPLib.List.insertP" f f_ood f_err
  where
    f (val,index,list) = x++(prepend val y)
      where
        (x,y) = (P.splitAt index list)
    f_ood (_,index,list) = P.concat [r1,r2]
      where
        r1 = if' (index < 0)              [(0,[(show index),(show (P.length list))])] []
        r2 = if' (index > (P.length list)) [(1,[(show index),(show (P.length list))])] []
    f_err = 
      [
        ["negative index ","; list has "," elements"],
        ["index "," out of bounds; list has "," elements (that is also the largest legal index)"]
      ]    
    
remove = unpack2 . apply $ removeP
removeD cl = unpack2 . (apply' ("SPLib.List.removeD @ "++cl)) $ removeP
removeP = PartialFunction "SPLib.List.removeP" f f_ood f_err
  where
    f (index,list) = x++(removeFirst y)
      where
        (x,y) = (P.splitAt index list) 
    f_ood (index,list) = P.concat [r1,r2]
      where
        r1 = if' (index < 0)              [(0,[(show index),(show (P.length list))])] []
        r2 = if' (index >= (P.length list)) [(1,[(show index),(show (P.length list))])] []
    f_err = 
      [
        ["negative index ","; list has "," elements"],
        ["index "," out of bounds; list has "," elements"]
      ]    
    
removeFirst = apply removeFirstP
removeFirstD cl = (apply' ("SPLib.List.removeFirstD @ "++cl)) removeFirstP
removeFirstP = PartialFunction "SPLib.List.removeFirstP" f f_ood f_err
  where
    f list = P.tail list
    f_ood list = if' (isEmpty list) [(0,[])] []
    f_err = [["empty list"]]
    
removeLast  = apply removeLastP
removeLastD cl = (apply' ("SPLib.List.removeLastD @ "++cl)) removeLastP
removeLastP = PartialFunction "SPLib.List.removeLastP" f f_ood f_err
  where
    f list = P.init list
    f_ood list = if' (isEmpty list) [(0,[])] []
    f_err = [["empty list"]]
        
contains  = P.elem
removeDuplicates = L.nub

reduce = unpack2 . apply $ reduceP
reduceD cl = unpack2 . (apply' ("SPLib.List.reduceD @ "++cl)) $ reduceP
reduceP = (PartialFunction "SPLib.List.reduceP" f f_ood f_err)
  where
    f (f,list) = P.foldl f (first list) (removeFirst list) 
    f_ood (_,list) = if' (isEmpty list) [(0,[])] []
    f_err = [["can't reduce empty list"]]

find      = unpack2 . apply $ findP
findD cl  = unpack2 . (apply' ("SPLib.List.findD @ "++cl)) $ findP
findIndex = unpack2 . apply $ findIndexP
findIndexD cl = unpack2 . (apply' ("SPLib.List.findIndexD @ "++cl)) $ findIndexP
findP = (PartialFunction "SPLib.List.findP" f f_ood f_err)
  where
    f (pred,list) = (just (L.find pred list))
    f_ood (pred,list) = if' (isNothing (L.find pred list)) [(0,[])] []
    f_err = [["no element found which matchtes the predicate"]]
findIndexP = (PartialFunction "SPLib.List.findIndexP" f f_ood f_err)
  where
    f (pred,list) = (just (L.findIndex pred list))
    f_ood (pred,list) = if' (isNothing (L.findIndex pred list)) [(0,[])] []
    f_err = [["no element found which matches the predicate"]]
    
join  = L.intercalate
split = unpack2 . apply $ splitP
splitD cl  = unpack2 . (apply' ("SPLib.List.splitD @ "++cl)) $ splitP
splitP = PartialFunction "SPLib.List.splitP" f f_ood f_err
  where
    f (needle,haystack) = split_f needle haystack
    f_ood (needle,haystack) = split_f_ood needle haystack
    f_err = [["empty needle"]]

splitAtFirst  = unpack2 . apply $ splitAtFirstP
splitAtFirstD cl  = unpack2 . (apply' ("SPLib.List.splitAtFirstD @ "++cl)) $ splitAtFirstP
splitAtFirstP = PartialFunction "SPLib.List.splitAtFirstP" f f_ood f_err
  where
    f (needle,haystack) = splitAtFirst_f needle haystack
    f_ood (needle,haystack) = splitAtFirst_f_ood needle haystack      
    f_err = 
      [
        ["empty needle"],
        ["needle not found"]
      ]
    
--START OF PROGRAM split

data Data_split  = Data_split 
  String --cur
  [Error] --errs
  String --haystack
  String --needle
  [String] --res
  Bool Bool Bool Bool Bool
  
--SIGNATURES
split_f :: String -> String -> [String]
split_f_ood :: String -> String -> [Error]

split_o_add :: Data_split -> Data_split
split_o_end :: Data_split -> Data_split
split_o_err :: Data_split -> Data_split
split_o_init :: Data_split -> Data_split
split_o_next :: Data_split -> Data_split
split_o_nop :: Data_split -> Data_split

split_p_emptyHaystack :: Data_split -> Bool
split_p_err :: Data_split -> Bool
split_p_foundNeedle :: Data_split -> Bool

--DEFINITIONS
split_f needle haystack = (traceProgramCall "split_f" "split" [("cur","String"),("errs","[Error]"),("haystack","String"),("needle","String"),("res","[String]")]  res')
  where
    (Data_split _ _ _ _ res' _ _ _ _ _) = (split_o_init (Data_split undefined undefined haystack needle undefined False False True True False))

split_f_ood needle haystack = (traceProgramCall "split_f_ood" "split" [("cur","String"),("errs","[Error]"),("haystack","String"),("needle","String"),("res","[String]")]  errs')
  where
    (Data_split _ errs' _ _ _ _ _ _ _ _) = (split_o_init (Data_split undefined undefined haystack needle undefined False False True True False))

split_p_emptyHaystack (Data_split cur errs haystack needle res _ _ _ _ _) = isEmpty haystack

split_p_err (Data_split cur errs haystack needle res _ _ _ _ _) = not $ isEmpty errs

split_p_foundNeedle (Data_split cur errs haystack needle res _ _ _ _ _) = (take (length needle) haystack) == needle

split_o_add (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_add" data_ [True,False,True,False,True] flow_)
  where
    cur' = []
    haystack' = drop (length needle) haystack
    res' = prepend (reverse cur) res
    errs' = errs
    needle' = needle
    cur_iDn_ = True
    haystack_iDn_ = True
    res_iDn_ = True
    errs_iDn_ = errs_iD_
    needle_iDn_ = needle_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "split" "o_nop" (split_o_nop data_))

split_o_end (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_end" data_ [False,False,False,False,True] flow_)
  where
    res' = reverse (prepend (reverse cur) res)
    cur' = cur
    errs' = errs
    haystack' = haystack
    needle' = needle
    res_iDn_ = True
    cur_iDn_ = cur_iD_
    errs_iDn_ = errs_iD_
    haystack_iDn_ = haystack_iD_
    needle_iDn_ = needle_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "split" data_)

split_o_err (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_err" data_ [False,False,False,False,False] flow_)
  where
    cur' = cur
    errs' = errs
    haystack' = haystack
    needle' = needle
    res' = res
    cur_iDn_ = cur_iD_
    errs_iDn_ = errs_iD_
    haystack_iDn_ = haystack_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "split" data_)

split_o_init (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_init" data_ [True,True,False,False,True] flow_)
  where
    cur' = []
    errs' = conditionalError (isEmpty needle) (0,[])
    res' = []
    haystack' = haystack
    needle' = needle
    cur_iDn_ = True
    errs_iDn_ = True
    res_iDn_ = True
    haystack_iDn_ = haystack_iD_
    needle_iDn_ = needle_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    p__ = (split_p_err data_)    
    flow_ = 
      (traceProgramPred "split" "p_err" p__ (if' p__
        (traceProgramNextOp "split" "o_err" (split_o_err data_))
        (traceProgramNextOp "split" "o_nop" (split_o_nop data_)) 
      ))

split_o_next (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_next" data_ [True,False,True,False,False] flow_)
  where
    cur' = prepend (first haystack) cur
    haystack' = removeFirst haystack
    errs' = errs
    needle' = needle
    res' = res
    cur_iDn_ = True
    haystack_iDn_ = True
    errs_iDn_ = errs_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "split" "o_nop" (split_o_nop data_))

split_o_nop (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = (traceProgramOp "split" "o_nop" data_ [False,False,False,False,False] flow_)
  where
    cur' = cur
    errs' = errs
    haystack' = haystack
    needle' = needle
    res' = res
    cur_iDn_ = cur_iD_
    errs_iDn_ = errs_iD_
    haystack_iDn_ = haystack_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_split cur' errs' haystack' needle' res' cur_iDn_ errs_iDn_ haystack_iDn_ needle_iDn_ res_iDn_)
    p__ = (split_p_emptyHaystack data_)
    p_1_ = (split_p_foundNeedle data_)    
    flow_ = 
      (traceProgramPred "split" "p_emptyHaystack" p__ (if' p__
        (traceProgramNextOp "split" "o_end" (split_o_end data_))
        (traceProgramPred "split" "p_foundNeedle" p_1_ (if' p_1_
          (traceProgramNextOp "split" "o_add" (split_o_add data_))
          (traceProgramNextOp "split" "o_next" (split_o_next data_)) 
        )) 
      ))

--SHOW
instance Show Data_split where  
  show (Data_split cur errs haystack needle res cur_iD_ errs_iD_ haystack_iD_ needle_iD_ res_iD_) = 
    (show [
      (if' cur_iD_ (prepend 'd' (show cur)) "u"),
      (if' errs_iD_ (prepend 'd' (show errs)) "u"),
      (if' haystack_iD_ (prepend 'd' (show haystack)) "u"),
      (if' needle_iD_ (prepend 'd' (show needle)) "u"),
      (if' res_iD_ (prepend 'd' (show res)) "u")
    ])

--END OF PROGRAM split

--START OF PROGRAM splitAtFirst

data Data_splitAtFirst  = Data_splitAtFirst 
  [Error] --errs
  String --haystack
  String --left
  String --needle
  (String,String) --res
  Bool Bool Bool Bool Bool
  
--SIGNATURES
splitAtFirst_f :: String -> String -> (String,String)
splitAtFirst_f_ood :: String -> String -> [Error]

splitAtFirst_o_err :: Data_splitAtFirst -> Data_splitAtFirst
splitAtFirst_o_found :: Data_splitAtFirst -> Data_splitAtFirst
splitAtFirst_o_init :: Data_splitAtFirst -> Data_splitAtFirst
splitAtFirst_o_next :: Data_splitAtFirst -> Data_splitAtFirst
splitAtFirst_o_nop :: Data_splitAtFirst -> Data_splitAtFirst

splitAtFirst_p_emptyHaystack :: Data_splitAtFirst -> Bool
splitAtFirst_p_emptyNeedle :: Data_splitAtFirst -> Bool
splitAtFirst_p_err :: Data_splitAtFirst -> Bool
splitAtFirst_p_found :: Data_splitAtFirst -> Bool

--DEFINITIONS
splitAtFirst_f needle haystack = (traceProgramCall "splitAtFirst_f" "splitAtFirst" [("errs","[Error]"),("haystack","String"),("left","String"),("needle","String"),("res","(String,String)")]  res')
  where
    (Data_splitAtFirst _ _ _ _ res' _ _ _ _ _) = (splitAtFirst_o_init (Data_splitAtFirst undefined haystack undefined needle undefined False True False True False))

splitAtFirst_f_ood needle haystack = (traceProgramCall "splitAtFirst_f_ood" "splitAtFirst" [("errs","[Error]"),("haystack","String"),("left","String"),("needle","String"),("res","(String,String)")]  errs')
  where
    (Data_splitAtFirst errs' _ _ _ _ _ _ _ _ _) = (splitAtFirst_o_init (Data_splitAtFirst undefined haystack undefined needle undefined False True False True False))

splitAtFirst_p_emptyHaystack (Data_splitAtFirst errs haystack left needle res _ _ _ _ _) = isEmpty haystack

splitAtFirst_p_emptyNeedle (Data_splitAtFirst errs haystack left needle res _ _ _ _ _) = isEmpty needle

splitAtFirst_p_err (Data_splitAtFirst errs haystack left needle res _ _ _ _ _) = not $ isEmpty errs

splitAtFirst_p_found (Data_splitAtFirst errs haystack left needle res _ _ _ _ _) = (take (length needle) haystack) == needle

splitAtFirst_o_err (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = (traceProgramOp "splitAtFirst" "o_err" data_ [False,False,False,False,False] flow_)
  where
    errs' = errs
    haystack' = haystack
    left' = left
    needle' = needle
    res' = res
    errs_iDn_ = errs_iD_
    haystack_iDn_ = haystack_iD_
    left_iDn_ = left_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_splitAtFirst errs' haystack' left' needle' res' errs_iDn_ haystack_iDn_ left_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "splitAtFirst" data_)

splitAtFirst_o_found (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = (traceProgramOp "splitAtFirst" "o_found" data_ [False,True,False,False,True] flow_)
  where
    haystack' = drop (length needle) haystack
    res' = ((reverse left), haystack')
    errs' = errs
    left' = left
    needle' = needle
    haystack_iDn_ = True
    res_iDn_ = True
    errs_iDn_ = errs_iD_
    left_iDn_ = left_iD_
    needle_iDn_ = needle_iD_
    data_ = (Data_splitAtFirst errs' haystack' left' needle' res' errs_iDn_ haystack_iDn_ left_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "splitAtFirst" data_)

splitAtFirst_o_init (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = (traceProgramOp "splitAtFirst" "o_init" data_ [True,False,True,False,False] flow_)
  where
    errs' = conditionalError (isEmpty needle) (0,[])
    left' = []
    haystack' = haystack
    needle' = needle
    res' = res
    errs_iDn_ = True
    left_iDn_ = True
    haystack_iDn_ = haystack_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_splitAtFirst errs' haystack' left' needle' res' errs_iDn_ haystack_iDn_ left_iDn_ needle_iDn_ res_iDn_)
    p__ = (splitAtFirst_p_err data_)    
    flow_ = 
      (traceProgramPred "splitAtFirst" "p_err" p__ (if' p__
        (traceProgramNextOp "splitAtFirst" "o_err" (splitAtFirst_o_err data_))
        (traceProgramNextOp "splitAtFirst" "o_nop" (splitAtFirst_o_nop data_)) 
      ))

splitAtFirst_o_next (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = (traceProgramOp "splitAtFirst" "o_next" data_ [False,True,True,False,False] flow_)
  where
    haystack' = removeFirst haystack
    left' = prepend (first haystack) left
    errs' = errs
    needle' = needle
    res' = res
    haystack_iDn_ = True
    left_iDn_ = True
    errs_iDn_ = errs_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_splitAtFirst errs' haystack' left' needle' res' errs_iDn_ haystack_iDn_ left_iDn_ needle_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "splitAtFirst" "o_nop" (splitAtFirst_o_nop data_))

splitAtFirst_o_nop (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = (traceProgramOp "splitAtFirst" "o_nop" data_ [True,False,False,False,False] flow_)
  where
    errs' = conditionalError (isEmpty haystack) (1,[])
    haystack' = haystack
    left' = left
    needle' = needle
    res' = res
    errs_iDn_ = True
    haystack_iDn_ = haystack_iD_
    left_iDn_ = left_iD_
    needle_iDn_ = needle_iD_
    res_iDn_ = res_iD_
    data_ = (Data_splitAtFirst errs' haystack' left' needle' res' errs_iDn_ haystack_iDn_ left_iDn_ needle_iDn_ res_iDn_)
    p__ = (splitAtFirst_p_err data_)
    p_1_ = (splitAtFirst_p_found data_)    
    flow_ = 
      (traceProgramPred "splitAtFirst" "p_err" p__ (if' p__
        (traceProgramNextOp "splitAtFirst" "o_err" (splitAtFirst_o_err data_))
        (traceProgramPred "splitAtFirst" "p_found" p_1_ (if' p_1_
          (traceProgramNextOp "splitAtFirst" "o_found" (splitAtFirst_o_found data_))
          (traceProgramNextOp "splitAtFirst" "o_next" (splitAtFirst_o_next data_)) 
        )) 
      ))

--SHOW
instance Show Data_splitAtFirst where  
  show (Data_splitAtFirst errs haystack left needle res errs_iD_ haystack_iD_ left_iD_ needle_iD_ res_iD_) = 
    (show [
      (if' errs_iD_ (prepend 'd' (show errs)) "u"),
      (if' haystack_iD_ (prepend 'd' (show haystack)) "u"),
      (if' left_iD_ (prepend 'd' (show left)) "u"),
      (if' needle_iD_ (prepend 'd' (show needle)) "u"),
      (if' res_iD_ (prepend 'd' (show res)) "u")
    ])

--END OF PROGRAM splitAtFirst
