module SPLib.PartialFunction (Error, PartialFunction(PartialFunction), apply, apply', applySafe, applyUnsafe, inDomain, oodReasons, functionName, compose, totalFunction, instantiateErrorMsg, getErrorsAsString, getErrorsAsIndentedString, conditionalError, unpack2, unpack3, unpack4, unpack5, unpack6, unpack7, unpack8, unpack9) where

import qualified Data.List (intercalate)

data PartialFunction a b = PartialFunction 
  FunctionName     --name of the function used for error messages
  (a -> b)         --the partial function
  (a -> [Error])   --list of reasons why input is outside of the domain, if empty then input is in domain                   
  [ErrMsg]         --error messages, referenced with ErrNo using the index (starting from 0) 
  
type ErrNo = Int
type ErrValue = String
type ErrMsg = [String]
type FunctionCallId = String
type FunctionName = String
type Error = (ErrNo,[ErrValue])

  {-
    Format of ErrMsg
  
    The elements of x::ErrMsg and y::[ErrValue] are intertwined. More specifically, y[i] is inserted between x[i] and x[i+1] (with i starting from 0). It must hold that |y| = |x| - 1.
    
    Example:
    x = ["Index ", " out of bounds in list ", " with length ", ""]
    y = ["5","[33,55]",2]
    ->  "Index 5 out of bounds in list [33,55] with length 2"
  -}

apply       :: (PartialFunction a b) -> (a -> b)
apply'      :: FunctionCallId -> (PartialFunction a b) -> (a -> b) --FunctionCallId can be used to identify where the call was made 
applySafe   :: FunctionCallId -> (PartialFunction a b) -> (a -> b) --same as apply'
applyUnsafe :: (PartialFunction a b) -> (a -> b) --returned function does not check for out of domain 

inDomain     :: (PartialFunction a b) -> a -> Bool
oodReasons   :: (PartialFunction a b) -> a -> [Error]  --returns list of reasons why input is out of domain
functionName :: (PartialFunction a b) -> FunctionName

compose       :: FunctionName -> (PartialFunction a b) -> (PartialFunction b c) -> (PartialFunction a c)
totalFunction :: (a -> b) -> (PartialFunction a b)

instantiateErrorMsg       :: (PartialFunction a b) -> Error -> String 
getErrorsAsString         :: (PartialFunction a b) -> a -> String
getErrorsAsIndentedString :: (PartialFunction a b) -> a -> String
conditionalError          :: Bool -> Error -> [Error]

unpack2 :: ((a1,a2) -> b)                      -> (a1 -> a2 -> b) 
unpack3 :: ((a1,a2,a3) -> b)                   -> (a1 -> a2 -> a3 -> b) 
unpack4 :: ((a1,a2,a3,a4) -> b)                -> (a1 -> a2 -> a3 -> a4 -> b) 
unpack5 :: ((a1,a2,a3,a4,a5) -> b)             -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) 
unpack6 :: ((a1,a2,a3,a4,a5,a6) -> b)          -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b) 
unpack7 :: ((a1,a2,a3,a4,a5,a6,a7) -> b)       -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b) 
unpack8 :: ((a1,a2,a3,a4,a5,a6,a7,a8) -> b)    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b) 
unpack9 :: ((a1,a2,a3,a4,a5,a6,a7,a8,a9) -> b) -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b) 


--DEFINITIONS

apply  = apply' ""
apply' = applySafe  
--apply' _ pf = applyUnsafe pf

applySafe callId pf x = 
  (if' (inDomain pf x) 
    (f x)
    (error errMsg)
  )
  where
    errMsg = (if' (isEmpty callId) (join "\n" x3) (join "\n" x2))
    x1 = (map (instantiateErrorMsg pf) reasons)
    x2 = (prepend callId x1)        
    x3 = (prepend f_name x1)        
    (PartialFunction f_name f _ _) = pf
    reasons = (oodReasons pf x)
    
applyUnsafe (PartialFunction _ f _ _) = f
  
inDomain (PartialFunction _ _ f_ood _) x = isEmpty (f_ood x)  
oodReasons (PartialFunction _ _ f_ood _) = f_ood
functionName (PartialFunction name _ _ _) = name
    
totalFunction f = (PartialFunction "" f (\_ -> []) [])

{-
  if the error number x is even then error came from f; otherwise from g
  x even: x/2    = error number in terms of f 
  x odd:  (x-1)/ = error number in terms of g
-}
compose name pf pg = (PartialFunction name h h_ood h_err)
  where
    h = g . f
    h_ood x = 
      (if' (inDomain pf x)
        (map pgt (oodReasons pg (f x))) --g errors for input (f x)
        (map pft (oodReasons pf x))     --f errors for input x
      )
      where
        pft (errNo,x) = ((fNewErrId errNo),x)
        pgt (errNo,x) = ((gNewErrId errNo),x)
                     
    (PartialFunction _ f _ f_err) = pf    
    (PartialFunction _ g _ g_err) = pg    
    
    fNewErrId x = 2 * x
    gNewErrId x = (2 * x) + 1
    h_err = (interleave (pad m f_err) (pad m g_err))
      where
        m = max (length f_err) (length g_err) 
    
instantiateErrorMsg pf (errId,errVals) =
  (if' validErrId
    (if' ((length errVals) == ((length errMsg)-1)) 
      (join "" (prepend (errNoTag errId) (prepend (first errMsg) (interleave errVals (removeFirst errMsg)))))
      (error err2) 
    )
    (error err1)
  )
  where
    errMsg = get errId f_errs
    
    (PartialFunction _ _ _ f_errs) = pf
    
    err1 = (errNoTag 0)++"PartialFunction.instantiateErrorMsg: invalid error id "++(show errId)++"; errVals = "++(show errVals)++"; errs = "++(show f_errs)
    err2 = (errNoTag 1)++"PartialFunction.instantiateErrorMsg: length of errVals = "++(show errVals)++" and errorMsg "++(show errMsg)++" does not match ("++(show (length errVals))++" != "++(show (length errMsg))++"-1); errs = "++(show f_errs) 
          
    validErrId = (errId >= 0) && (errId < (length f_errs))    
    errNoTag x = "[ErrNo:"++(show errId)++"] "

getErrorsAsString pf inp = join "\n" $ prepend z $ map (instantiateErrorMsg pf) $ (oodReasons pf) inp
  where
    z = "["++(functionName pf)++"]"
getErrorsAsIndentedString pf inp = join "\n" $ prepend z $ map (g . (instantiateErrorMsg pf)) $ (oodReasons pf) inp
  where
    z = "\t["++(functionName pf)++"]"
    g str = join "\n" $ map g2 $ split "\n" str 
      where
        g2 str = prepend '\t' str
        
conditionalError x y = if' x [y] []
    
unpack2 f = f'
  where
    f' x1 x2 = f (x1,x2)

unpack3 f = f'
  where
    f' x1 x2 x3 = f (x1,x2,x3)

unpack4 f = f'
  where
    f' x1 x2 x3 x4 = f (x1,x2,x3,x4)

unpack5 f = f'
  where
    f' x1 x2 x3 x4 x5 = f (x1,x2,x3,x4,x5)

unpack6 f = f'
  where
    f' x1 x2 x3 x4 x5 x6 = f (x1,x2,x3,x4,x5,x6)

unpack7 f = f'
  where
    f' x1 x2 x3 x4 x5 x6 x7 = f (x1,x2,x3,x4,x5,x6,x7)

unpack8 f = f'
  where
    f' x1 x2 x3 x4 x5 x6 x7 x8 = f (x1,x2,x3,x4,x5,x6,x7,x8)

unpack9 f = f'
  where
    f' x1 x2 x3 x4 x5 x6 x7 x8 x9 = f (x1,x2,x3,x4,x5,x6,x7,x8,x9)
    
if'  :: Bool -> a -> a -> a 
if' True  x _ = x
if' False _ y = y

first = head
removeFirst = tail
get i l  = l!!i
isEmpty = null
prepend x l = x:l
join = Data.List.intercalate   

pad :: Int -> [a] -> [a]
pad x l = (if' (diff > 0) l' l)
  where
    diff = x - (length l)
    l' = pad (x-1) (l ++ [undefined])

interleave :: [a] -> [a] -> [a]     
interleave x y = (if' (isEmpty x) [] x2)    
  where
    x0 = (interleave (removeFirst x) (removeFirst y))
    x1 = (prepend (first y) x0)
    x2 = (prepend (first x) x1) 
    
    
    
--START OF PROGRAM split

data Data_split  = Data_split 
  String --cur
  [Error] --errs
  String --haystack
  String --needle
  [String] --res
  Bool Bool Bool Bool Bool
  
--SIGNATURES
split :: String -> String -> [String]

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
split needle haystack = (traceProgramCall "split" "split" [("cur","String"),("errs","[Error]"),("haystack","String"),("needle","String"),("res","[String]")]  res')
  where
    (Data_split _ _ _ _ res' _ _ _ _ _) = (split_o_init (Data_split undefined undefined haystack needle undefined False False True True False))

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


trace str x = x  

traceProgramCall fName pName vars x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PC "++(show (fName,pName,vars))

traceProgramOp pName opName curValues reassignedList x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PO "++(show (pName,opName,curValues,reassignedList))

traceProgramPred pName predName predTrue x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PP "++(show (pName,predName,predTrue))

traceProgramNextOp pName opName x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PN "++(show (pName,opName))

traceProgramHalt pName x = if' (shouldProgramBeTraced pName) (trace str x) x
  where
    str = "PH "++(show pName)

-- Tracing filter     
shouldProgramBeTraced pName = False
