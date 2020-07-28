module SPLib where

import Debug.Trace
import qualified Data.List (elemIndex, intercalate, find, findIndex) 
import qualified Data.Maybe (isNothing)
import qualified Text.Read (readMaybe)
import qualified Data.Char (isSpace)

-- Operators &&, ||, /=, ==, <=, >=, <, >, +, *, ^, not, ++

if'       :: Bool -> a -> a -> a
just      :: (Maybe a) -> a 
isNothing :: (Maybe a) -> Bool
readMaybe :: Read a => String -> Maybe a

--map     :: (a -> b) -> [a] -> [b]         ; in Prelude
--filter  :: (a -> Bool) -> [a]             ; in Prelude
reduce    :: (a -> a -> a) -> [a] -> a 
--zip     :: [a] -> [b] -> [(a,b)]          ; in Prelude
--find    ::  Foldable t => (a -> Bool) -> t a -> Maybe a
find      ::  (a -> Bool) -> [a] -> (Maybe a)
findIndex ::  (a -> Bool) -> [a] -> (Maybe Int)

get    :: [a] -> Int -> a 
first  :: [a] -> a 
--last :: [a] -> a                         ; in Prelude

--length   :: [a] -> Int                   ; in Prelude
contains   :: (Eq a) => [a] -> a -> Bool  -- contains2 list el offset = (contains (drop offset list) el)
indexOf    :: (Eq a) => [a] -> a -> Int   -- indexOf2  list el offset = (indexOf (drop offset list) el)
isEmpty      :: [a] -> Bool

append       :: [a] -> a -> [a]
prepend      :: [a] -> a -> [a]
set          :: [a] -> Int -> a -> [a]
insert       :: [a] -> Int -> a -> [a]
remove       :: [a] -> Int -> [a]
removeFirst  :: [a] -> [a]
removeLast   :: [a] -> [a]
sublist      :: [a] -> Int -> Int -> [a]
--reverse    :: [a] -> [a]                 ; in Prelude

--take       :: Int -> [a] -> [a]          ; in Prelude; takes first k elements of list
--drop       :: Int -> [a] -> [a]          ; in Prelude; drops first k elements from list
--splitAt    :: Int -> [a] -> ([a],[a])    ; in Prelude; splitAt n lst = ((take n lst),(drop n lst)) 

strPos       :: String -> String -> Int -> (Maybe Int) --haystack, needle, offset 
trim         :: String -> String --removes whitespaces and tabs from left and right side
trimr        :: String -> String --removes whitespaces and tabs from right side

join         :: String -> [String] -> String           --separator, list
split        :: String -> String -> [String]           --separator, text
splitAtFirst :: String -> String -> (String,String)    --separator, text
isSpace      :: Char -> Bool
startsWith   :: (Eq a) => [a] -> [a] -> Bool           --needle,haystack
endsWith     :: (Eq a) => [a] -> [a] -> Bool           --needle,haystack

-- DEFINITIONS

if' True  x _ = x
if' False _ y = y

just (Just x) = x
just Nothing = (error "expected (Just x) but got Nothing")

isNothing = Data.Maybe.isNothing
readMaybe = Text.Read.readMaybe

find = Data.List.find
findIndex = Data.List.findIndex

reduce f list = 
    (if' ((length list) == 1)
        (head list)
        (reduce f 
            ( (f (head list) (get list 1)) : (tail (tail list)))
        )    
    )    

get list index = (list!!index)
first list = (list!!0)
contains list val = (elem val list)  
indexOf list val = (just (Data.List.elemIndex val list))
isEmpty list = (null list)
append list val = (list++[val])
prepend list val = (val:list)
removeFirst list = (tail list)
removeLast list = (init list)

sublist list start len = (if' ((start + len) <= (length list)) (take len (drop start list)) (error errMsg))
  where
    errMsg = "start+len > (length list)"++errArgs
    errArgs = "; (list,start,len)=("++(join "," ["N/A",(show start),(show len)])++")"

set list index val = x++(prepend (removeFirst y) val)
  where
    (x,y) = (splitAt index list)
insert list index val = x++(prepend y val)
  where
    (x,y) = (splitAt index list)
remove list index = x++(removeFirst y)
  where
    (x,y) = (splitAt index list)    

strPos haystack "" offset = (error errMsg)
  where
    errMsg = "empty needle"++errArgs
    errArgs = "; (haystack,needle,offset)=("++(join "," [(show haystack),(show ""),(show offset)])++")"
    
strPos haystack needle offset = 
  (if' (offset < 0) (error errMsg)
    (if' (offset >= (length haystack)) Nothing    
      (if' (offset > 0) 
        normalize
        (if' ((length haystack)<(length needle))
          Nothing 
          (if' (containsFirstNeedleChar && restLongEnough)
            (if' match
              (Just posFirstNeedleChar)
              (if' ((posFirstNeedleChar+1)>=(length haystack))
                Nothing
                (strPos haystack needle (posFirstNeedleChar+1))
              )
            )
            Nothing
          )
        )                    
      )
    )    
  )
  where
    normalizeM = (strPos (drop offset haystack) needle 0) 
    normalize = (if' (isNothing normalizeM) Nothing (Just ((just normalizeM)+offset)))    
    containsFirstNeedleChar = (contains haystack (first needle))    
    posFirstNeedleChar = (indexOf haystack (first needle))
    restLongEnough = ((length (drop posFirstNeedleChar haystack)) >= (length needle))    
    match = (needle == (sublist haystack posFirstNeedleChar (length needle)))           
    errMsg = "offset < 0"++errArgs
    errArgs = "; (haystack,needle,offset)=("++(join "," [(show haystack),(show needle),(show offset)])++")"

trim = f . f
   where f = reverse . dropWhile Data.Char.isSpace    

trimr = reverse . (dropWhile Data.Char.isSpace) . reverse 
   
splitAtFirst sep str = 
  (if' (isNothing posM) 
    (error errMsg)
    ((sublist str 0 pos), (drop (pos+(length sep)) str))
  )
  where
    posM = (strPos str sep 0)
    pos = (just posM)    
    errMsg = "str does not contain sep"++errArgs
    errArgs = "; (sep,str)=("++(join "," [(show sep),(show str)])++")"    
    
join sep list = (Data.List.intercalate sep list)

split sep str = (if' (noSep) [str] (prepend (split sep y) x) )
  where
    noSep = (isNothing (strPos str sep 0))
    (x,y) = (splitAtFirst sep str)

  
isSpace = Data.Char.isSpace

endsWith y x = 
  (if' ((length y) > (length x)) False
    ((drop ((length x)-(length y)) x) == y))
    
startsWith y x = 
  (if' ((length y) > (length x)) False
    ((take (length y) x) == y))
    
dropAtEnd i x = (take ((length x)-i) x)
    
-- functions for program tracing  

traceProgramCall :: String -> String -> [(String,String)] -> a -> a
traceProgramOp :: (Show a) => String -> String -> a -> [Bool] -> a -> a
--traceProgramOp :: (Show a) => String -> String -> a -> a -> a
traceProgramPred :: String -> String -> Bool -> a -> a
traceProgramNextOp :: String -> String -> a -> a
traceProgramHalt :: String -> a -> a

traceProgramCall fName pName vars x = (trace' str x)
  where
    str = "PC "++(show (fName,pName,vars))

traceProgramOp pName opName curValues reassignedList x = (trace' str x)
  where
    str = "PO "++(show (pName,opName,curValues,reassignedList))

traceProgramPred pName predName predTrue x = (trace' str x)
  where
    str = "PP "++(show (pName,predName,predTrue))

traceProgramNextOp pName opName x = (trace' str x)
  where
    str = "PN "++(show (pName,opName))

traceProgramHalt pName x = (trace' str x)
  where
    str = "PH "++(show pName)

trace' str x = x               --DISABLE TRACE
--trace' str x = (trace str x) --ENABLE TRACE 