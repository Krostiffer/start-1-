module SPLib.Basic (Eq((==),(/=)), Show(show), Read(readsPrec), Ord((<),(<=),(>),(>=),max,min), Enum (succ, pred, toEnum, fromEnum), Bool(True,False), Int, Char, String, Maybe(Nothing,Just), Either(Left,Right), (&&), (||), (+), (-), (*), (^), (.), (++), ($), if', not, id, undefined, error, SPLib.Basic.read, readD, readP, mod, SPLib.Basic.div, divD, divP, fst, snd, isNothing, isJust, just, justD, justP, isLeft, isRight, left, leftD, leftP, right, rightD, rightP, trace, traceProgramCall, traceProgramOp, traceProgramPred, traceProgramNextOp, traceProgramHalt) where

import Prelude as P (Eq((==),(/=)), Show(show), Read(readsPrec), Ord((<),(<=),(>),(>=),max,min), Enum (succ, pred, toEnum, fromEnum), Bool(True, False), Int, Char, String, Maybe(Nothing,Just), Either(Left,Right), (&&), (||), (+), (-), (*), (^), (.), (++), ($), not, undefined, error, mod, div, max, min, fst, snd, id, elem, read) 

import Text.Read (readMaybe)
import qualified Debug.Trace

import SPLib.PartialFunction

type CallLocation = String

--SIGNATURES

if'   :: Bool -> a -> a -> a  
--not :: Bool -> Bool

--undefined :: a
--error     :: String -> a

--show    :: Show a => a -> String
read      :: Read a => String -> a
readD     :: Read a => CallLocation -> String -> a
readM     :: Read a => String -> Maybe a
readP     :: Read a => PartialFunction String a

--max  :: Int -> Int -> Int
--min  :: Int -> Int -> Int
--mod  :: Int -> Int -> Int
div    :: Int -> Int -> Int
divD   :: CallLocation -> Int -> Int -> Int
divP   :: PartialFunction (Int, Int) Int
-- Invariant: (y * (div x y)) + (mod x y) = x

--fst :: (a,b) -> a
--snd :: (a,b) -> b


-- *** Maybe ***
isNothing :: Maybe a -> Bool
isJust    :: Maybe a -> Bool
just      :: Maybe a -> a
justD     :: CallLocation -> Maybe a -> a
justP     :: PartialFunction (Maybe a) a


-- *** Either ***
isLeft  :: Either a b -> Bool
isRight :: Either a b -> Bool
left    :: Either a b -> a
leftD   :: CallLocation -> Either a b -> a
right   :: Either a b -> b
rightD  :: CallLocation -> Either a b -> b
leftP   :: PartialFunction (Either a b) a
rightP  :: PartialFunction (Either a b) b


-- *** Tracing ***
trace              :: String -> a -> a 
traceProgramCall   :: String -> String -> [(String,String)] -> a -> a
traceProgramOp     :: (Show a) => String -> String -> a -> [Bool] -> a -> a
traceProgramPred   :: String -> String -> Bool -> a -> a
traceProgramNextOp :: String -> String -> a -> a
traceProgramHalt   :: String -> a -> a


-- DEFINITIONS 

if' True  x _ = x
if' False _ y = y

read = apply readP
readD cl = apply' ("SPLib.Basic.readD @ "++cl) readP
readM = readMaybe
readP = PartialFunction "SPLib.Basic.readP" f f_ood f_err
  where
    f = just . readMaybe'
    f_ood x = if' ((isNothing . readMaybe') x) [(0,[])] []
    f_err = [["parsing failed"]]
    
    readMaybe' = readMaybe :: (Read a => String -> Maybe a)


div = unpack2 . apply $ divP
divD cl = unpack2 . (apply' ("SPLib.Basic.divD @ "++cl)) $ divP
divP = (PartialFunction "SPLib.Basic.divP" f f_ood f_err)
  where
    f (x,y) = P.div x y --pack2 ((P.div)::(Int -> Int -> Int))
    f_ood (_,divisor) = if' (divisor==0) [(0,[])] []
    f_err = [["division by zero"]]

isNothing Nothing = True
isNothing _       = False 
isJust = not . isNothing 
just = apply justP
justD cl = apply' ("SPLib.Basic.justD @ "++cl) justP
justP = (PartialFunction "SPLib.Basic.justP" f f_ood f_err)
  where
    f (Just x) = x
    f_ood x = if' (isNothing x) [(0,[])] []
    f_err = [["expected Just, got Nothing"]]

isLeft (Left _) = True
isLeft (Right _) = False
isRight = not . isLeft
left = apply leftP
leftD cl = apply' ("SPLib.Basic.leftD @ "++cl) leftP
right = apply rightP
rightD cl = apply' ("SPLib.Basic.rightD @ "++cl) rightP
leftP = (PartialFunction "SPLib.Basic.leftP" f f_ood f_err)
  where
    f (Left x) = x
    f_ood x = if' (isRight x) [(0,[])] []
    f_err = [["expected Left, got Right"]]
rightP = (PartialFunction "SPLib.Basic.rightP" f f_ood f_err)
  where
    f (Right x) = x
    f_ood x = if' (isLeft x) [(0,[])] []
    f_err = [["expected Right, got Left"]]

trace str x = Debug.Trace.trace str x  

traceProgramCall fName pName vars x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PC "++(P.show (fName,pName,vars))

traceProgramOp pName opName curValues reassignedList x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PO "++(P.show (pName,opName,curValues,reassignedList))

traceProgramPred pName predName predTrue x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PP "++(P.show (pName,predName,predTrue))

traceProgramNextOp pName opName x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PN "++(P.show (pName,opName))

traceProgramHalt pName x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PH "++(P.show pName)

-- Tracing filter     

-- Option 1: trace only those programs whose name is not in the black list; empty black list => all programs are traced
{-shouldProgramBeTraced pName = not (elem pName blackList)
  where    
    blackList = ["split","splitAtFirst","escape","unescape","template","eval2","eval3"]
-}

-- Option 2: trace only those programs whose name is in the white list
shouldProgramBeTraced pName = elem pName whiteList 
  where   
    whiteList = ["determinize"]
