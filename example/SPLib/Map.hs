module SPLib.Map (
  SPLib.Map.Map, 
  isEmpty, size, SPLib.Map.empty, hasKey, get, getD, getP, set, remove, fromList, toList
  ) where

import Data.Map as M (Map, size, member, null, lookup, insert, toList, fromList, empty, delete)

import Prelude ()
import SPLib.Basic
import SPLib.PartialFunction

type Map = M.Map 
type CallLocation = String

--SIGNATURES

isEmpty :: M.Map k v -> Bool
--size    :: M.Map k v -> Int 
empty   :: M.Map k v

hasKey :: (Ord k) => k -> M.Map k v -> Bool
get    :: (Ord k, Show k) => k -> M.Map k v -> v
getD   :: (Ord k, Show k) => CallLocation -> k -> M.Map k v -> v
getP   :: (Ord k, Show k) => PartialFunction (k, M.Map k v) v
set    :: (Ord k) => v -> k -> M.Map k v -> M.Map k v
remove :: (Ord k) => k -> M.Map k v -> M.Map k v

--fromList :: Ord k => [(k,v)] -> M.Map k v
--toList   :: M.Map k v -> [(k,v)]


--DEFINITIONS

isEmpty = M.null
empty   = M.empty

hasKey  = M.member 
get     = (unpack2 . apply) getP
getD cl = (unpack2 . (apply' ("SPLib.Map.getD @ "++cl))) getP
getP    = (PartialFunction "SPLib.Map.getP" f f_ood f_err)
  where
    f (k,m) = (just (M.lookup k m))
    f_ood (k,m) = if' (isNothing (M.lookup k m)) [(0,[(show k)])] []
    f_err = [["key '","' not found"]]
    
set v k m = M.insert k v m 
remove = M.delete 

 