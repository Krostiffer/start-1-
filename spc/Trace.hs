module Trace where

import Data.Map (Map)
import Data.Either
import qualified Data.Map as Map

import SPLib
import qualified LabeledTree as LT
import Template

data Trace = Trace 
  String --fname
  String --pname
  [(String,String)] --(varName,varType)
  Bool  --finished execution
  [TraceRow] deriving (Eq, Show, Read)

type Call = [Int] 

data TraceRow = TraceRow
  String --opName
  [String] --values
  [Bool] --reassignedList
  [(String,Bool)] --pred sequence
  (Maybe String) --next op (if it got so far)
  Int --line in source trace where PO occurred
  [Call]
   deriving (Eq, Show, Read)

initTrace :: String -> String -> [(String,String)] -> Trace  
addTraceRow :: Trace -> TraceRow -> Trace  
lastTraceRow :: Trace -> TraceRow
removeLastTraceRow :: Trace -> Trace
setTraceFinished :: Trace -> Trace
isTraceFinished :: Trace -> Bool

initTraceRow :: String -> [String] -> [Bool] -> Int -> [Call] -> TraceRow 
setNextOp :: TraceRow -> String -> TraceRow
addPred :: TraceRow -> String -> Bool -> TraceRow
addCalls :: TraceRow -> [Call] -> TraceRow

initTrace fName pName vars = (Trace fName pName vars False [])
addTraceRow (Trace x1 x2 x3 x4 rows) row = (Trace x1 x2 x3 x4 (append rows row))
lastTraceRow (Trace _ _ _ _ rows) = (last rows)
removeLastTraceRow (Trace x1 x2 x3 x4 rows) = (Trace x1 x2 x3 x4 (removeLast rows))
setTraceFinished (Trace x1 x2 x3 x4 x5) = (Trace x1 x2 x3 True x5)
isTraceFinished (Trace _ _ _ x4 _) = x4

initTraceRow opName values reassignedList srcLine calls = (TraceRow opName values reassignedList [] Nothing srcLine calls)
setNextOp (TraceRow x1 x2 x3 x4 _ x6 x7) nextOp = (TraceRow x1 x2 x3 x4 (Just nextOp) x6 x7) 
addPred (TraceRow x1 x2 x3 x4 x5 x6 x7) predName truth = (TraceRow x1 x2 x3 (append x4 (predName, truth)) x5 x6 x7)
addCalls (TraceRow x1 x2 x3 x4 x5 x6 x7) calls = (TraceRow x1 x2 x3 x4 x5 x6 (x7++calls))

traceToTemplateData :: String -> Trace -> [String] -> LT.LabeledTree


traceToTemplateData csvSeparator (Trace fName pName vars finished rows) other = res --returns [c]
  where     
    res = (LT.LabeledTree "data" [
            (makeRecord "FunctionName" fName),        
            (makeRecord "ProgramName" pName), 
            (makeRecord "Separator" csvSeparator),        
            (makeDataSet "Others" ds_others),
            (makeDataSet "Variables" ds_vars),
            (makeDataSet "Rows" ds_rows)
          ])
    
    ds_others = (map f other)
      where
        f line = (LT.LabeledTree "data" [(makeRecord "OtherLine" line)])
        
    ds_vars = (map f vars)
      where
        f (vName,typ) = (LT.LabeledTree "data" [(makeRecord "VariableName" vName)])
          
    ds_rows = (map f rows2)
      where
        rows2 = (zip rows (prepend (map Just rows) Nothing))
        f ((TraceRow opName values reassignedList predSeq nextOpM srcLine calls),prevRowM) =         
          (LT.LabeledTree "data" 
            (f1 [
              (makeRecord "Separator" csvSeparator), 
              (makeRecord "OperationName" opName),            
              (makeRecord "SourceLine" (show srcLine)),            
              (makeDataSet "Values" (ds_values values valuesPrev2 reassignedList)), --after values comes valuesPrev2
              (makeDataSet "Predicates" (ds_predseq predSeq)),
              (makeDataSet "Calls" (ds_calls calls))
            ])
          )
          where
            f1 x = (if' (isNothing nextOpM) x (prepend x (makeRecord "NextOperation" (just nextOpM))))      
            (Just (TraceRow _ valuesPrev _ _ _ _ _)) = prevRowM            
            valuesPrev2 = (if' (isNothing prevRowM) Nothing (Just valuesPrev))

    ds_calls calls = (map f calls)
      where
        f call = (LT.LabeledTree "data" [(makeRecord "Call" (format call))])        
        format x = (join "-" (map show x))
            
    ds_values values Nothing reassignedList = (map f values)
      where
        f val = (LT.LabeledTree "data" [(makeRecord "Value" val),z])
        z = (makeRecord "HasChanged" "True") 
    
    ds_values values (Just valuesPrev) reassignedList = (map f (zip (zip values valuesPrev) reassignedList))
      where
        f ((val,valPrev),reassigned) = (LT.LabeledTree "data" (f1 [(makeRecord "Value" val)]))
          where
            --if reassigned then check if value has changed compared to last one 
            f1 x = (if' (reassigned && (val /= valPrev)) (prepend x z) x)
            --f1 x = (if' reassigned (prepend x z) x)
            z = (makeRecord "HasChanged" "True")           
          
    ds_predseq predSeq = (map f predSeq)
      where
        f (predName,truth) = (LT.LabeledTree "data" [           
                               (makeRecord "PredicateName" predName),
                               (makeRecord "Truth" (bool2str truth))
                             ])
        bool2str x = (if' x "1" "0")

--traceToTemplateData (Trace fName pName vars finished rows) = undefined

 
type Path = [Int]
data TraceTree = TraceTree Trace [TraceTree] deriving (Show)

rootDegree :: TraceTree -> Int
rootLabel :: TraceTree -> Trace

getLabel :: TraceTree -> Path -> Trace 
setLabel :: TraceTree -> Path -> Trace -> TraceTree
degree   :: TraceTree -> Path -> Int
 
subtree :: TraceTree -> Path -> TraceTree 
pathExists :: TraceTree -> Path -> Bool 

appendSubtree :: TraceTree -> Path -> TraceTree -> TraceTree
allNodes :: TraceTree -> [Path]

rootDegree (TraceTree _ c) = (length c)
rootLabel (TraceTree l _) = l

subtree (TraceTree l c) p = (if' (isEmpty p) (TraceTree l c) (subtree (get c (first p)) (removeFirst p)) ) 
getLabel (TraceTree l c) p = (if' (isEmpty p) l (getLabel (get c (first p)) (removeFirst p)) ) 
pathExists (TraceTree l c) p = 
  (if' (isEmpty p) 
    True 
    (if' (((first p) >= 0) && ((first p) < (length c)))
      (pathExists (get c (first p)) (removeFirst p)) 
      False    
    ) 
  )

genericModifyTree  :: (TraceTree -> TraceTree) -> TraceTree -> Path -> TraceTree
genericModifyTree f (TraceTree l c) p = 
  (if' (isEmpty p)
      (f (TraceTree l c))
      (TraceTree l c')
  )
  where
    c' = (set c (first p) (genericModifyTree f (get c (first p)) (removeFirst p)))

  
setLabel tree path label = (genericModifyTree f tree path)
  where
    f (TraceTree _ c) = (TraceTree label c)
    
appendSubtree tree path subtree = (genericModifyTree f tree path)
  where
    f (TraceTree l c) = (TraceTree l (append c subtree))

degree t p = (rootDegree (subtree t p))

allNodes t = (allNodes_h t [])

allNodes_h :: TraceTree -> Path -> [Path]    
allNodes_h (TraceTree _ c) cur = 
  (if' (isEmpty c) 
    [cur] 
    (prepend  
      (reduce (++) (map (\(i,t) -> (allNodes_h t (append cur i))) c'))
      cur
    )
  )
  where
    c' = (zip [0..((length c)-1)] c)


--START OF PROGRAM parseTrace

data Data_parseTrace  = Data_parseTrace 
  Int --callCounter
  Int --callsStored
  [Int] --curPath
  Bool --err
  String --errMsg
  Bool --firstPC
  String --line
  [String] --lines
  Int --lno
  [String] --otherLines
  TraceTree --result
  Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
parseTrace :: [String] -> [Int] -> Int -> TraceTree
parseTraceLno :: [String] -> [Int] -> Int -> Int
parseTraceOther :: [String] -> [Int] -> Int -> [String]
parseTraceRest :: [String] -> [Int] -> Int -> [String]
parseTrace_error :: [String] -> [Int] -> Int -> Bool
parseTrace_errorMsg :: [String] -> [Int] -> Int -> String

parseTrace_o_err1 :: Data_parseTrace -> Data_parseTrace
parseTrace_o_init :: Data_parseTrace -> Data_parseTrace
parseTrace_o_next :: Data_parseTrace -> Data_parseTrace
parseTrace_o_other :: Data_parseTrace -> Data_parseTrace
parseTrace_o_pc :: Data_parseTrace -> Data_parseTrace
parseTrace_o_pc_rec :: Data_parseTrace -> Data_parseTrace
parseTrace_o_ph :: Data_parseTrace -> Data_parseTrace
parseTrace_o_pn :: Data_parseTrace -> Data_parseTrace
parseTrace_o_po :: Data_parseTrace -> Data_parseTrace
parseTrace_o_pp :: Data_parseTrace -> Data_parseTrace

parseTrace_p_emptyLine :: Data_parseTrace -> Bool
parseTrace_p_eol :: Data_parseTrace -> Bool
parseTrace_p_firstPC :: Data_parseTrace -> Bool
parseTrace_p_pc :: Data_parseTrace -> Bool
parseTrace_p_ph :: Data_parseTrace -> Bool
parseTrace_p_pn :: Data_parseTrace -> Bool
parseTrace_p_po :: Data_parseTrace -> Bool
parseTrace_p_pp :: Data_parseTrace -> Bool

--DEFINITIONS
parseTrace lines curPath lno = (traceProgramCall "parseTrace" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  result')
  where
    (Data_parseTrace _ _ _ _ _ _ _ _ _ _ result' _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTraceLno lines curPath lno = (traceProgramCall "parseTraceLno" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  lno')
  where
    (Data_parseTrace _ _ _ _ _ _ _ _ lno' _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTraceOther lines curPath lno = (traceProgramCall "parseTraceOther" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  otherLines')
  where
    (Data_parseTrace _ _ _ _ _ _ _ _ _ otherLines' _ _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTraceRest lines curPath lno = (traceProgramCall "parseTraceRest" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  lines')
  where
    (Data_parseTrace _ _ _ _ _ _ _ lines' _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTrace_error lines curPath lno = (traceProgramCall "parseTrace_error" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  err')
  where
    (Data_parseTrace _ _ _ err' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTrace_errorMsg lines curPath lno = (traceProgramCall "parseTrace_errorMsg" "parseTrace" [("callCounter","Int"),("callsStored","Int"),("curPath","[Int]"),("err","Bool"),("errMsg","String"),("firstPC","Bool"),("line","String"),("lines","[String]"),("lno","Int"),("otherLines","[String]"),("result","TraceTree")]  errMsg')
  where
    (Data_parseTrace _ _ _ _ errMsg' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseTrace_o_init (Data_parseTrace undefined undefined curPath undefined undefined undefined undefined lines lno undefined undefined False False True False False False False True True False False))

parseTrace_p_emptyLine (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (isEmpty (trim line))

parseTrace_p_eol (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (isEmpty lines)

parseTrace_p_firstPC (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = firstPC

parseTrace_p_pc (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (startsWith "PC " line)

parseTrace_p_ph (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (startsWith "PH " line)

parseTrace_p_pn (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (startsWith "PN " line)

parseTrace_p_po (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (startsWith "PO " line)

parseTrace_p_pp (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result _ _ _ _ _ _ _ _ _ _ _) = (startsWith "PP " line)

parseTrace_o_err1 (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_err1" data_ [False,False,False,True,True,False,False,False,False,False,False] flow_)
  where
    err' = True
    errMsg' = "no initial 'PO ' line found"
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    result' = result
    err_iDn_ = True
    errMsg_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    result_iDn_ = result_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseTrace" data_)

parseTrace_o_init (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_init" data_ [True,True,False,True,False,True,False,False,False,True,False] flow_)
  where
    callCounter' = 0
    callsStored' = 0
    err' = False
    firstPC' = False
    otherLines' = []
    curPath' = curPath
    errMsg' = errMsg
    line' = line
    lines' = lines
    lno' = lno
    result' = result
    callCounter_iDn_ = True
    callsStored_iDn_ = True
    err_iDn_ = True
    firstPC_iDn_ = True
    otherLines_iDn_ = True
    curPath_iDn_ = curPath_iD_
    errMsg_iDn_ = errMsg_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    result_iDn_ = result_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

parseTrace_o_next (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_next" data_ [False,False,False,False,False,False,True,True,True,False,False] flow_)
  where
    line' = (first lines)
    lines' = (removeFirst lines)
    lno' = lno + 1
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    otherLines' = otherLines
    result' = result
    line_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    otherLines_iDn_ = otherLines_iD_
    result_iDn_ = result_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_firstPC data_)
    p_0_ = (parseTrace_p_pc data_)
    p_0_1_ = (parseTrace_p_po data_)
    p_0_1_1_ = (parseTrace_p_pp data_)
    p_0_1_1_1_ = (parseTrace_p_pn data_)
    p_0_1_1_1_1_ = (parseTrace_p_ph data_)
    p_0_1_1_1_1_1_ = (parseTrace_p_emptyLine data_)
    p_0_1_1_1_1_1_0_ = (parseTrace_p_eol data_)
    p_1_ = (parseTrace_p_pc data_)
    p_1_1_ = (parseTrace_p_emptyLine data_)
    p_1_1_0_ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_firstPC" p__ (if' p__
        (traceProgramPred "parseTrace" "p_pc" p_0_ (if' p_0_
          (traceProgramNextOp "parseTrace" "o_pc_rec" (parseTrace_o_pc_rec data_))
          (traceProgramPred "parseTrace" "p_po" p_0_1_ (if' p_0_1_
            (traceProgramNextOp "parseTrace" "o_po" (parseTrace_o_po data_))
            (traceProgramPred "parseTrace" "p_pp" p_0_1_1_ (if' p_0_1_1_
              (traceProgramNextOp "parseTrace" "o_pp" (parseTrace_o_pp data_))
              (traceProgramPred "parseTrace" "p_pn" p_0_1_1_1_ (if' p_0_1_1_1_
                (traceProgramNextOp "parseTrace" "o_pn" (parseTrace_o_pn data_))
                (traceProgramPred "parseTrace" "p_ph" p_0_1_1_1_1_ (if' p_0_1_1_1_1_
                  (traceProgramNextOp "parseTrace" "o_ph" (parseTrace_o_ph data_))
                  (traceProgramPred "parseTrace" "p_emptyLine" p_0_1_1_1_1_1_ (if' p_0_1_1_1_1_1_
                    (traceProgramPred "parseTrace" "p_eol" p_0_1_1_1_1_1_0_ (if' p_0_1_1_1_1_1_0_
                      (traceProgramHalt "parseTrace" data_)
                      (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
                    ))
                    (traceProgramNextOp "parseTrace" "o_other" (parseTrace_o_other data_)) 
                  )) 
                )) 
              )) 
            )) 
          )) 
        ))
        (traceProgramPred "parseTrace" "p_pc" p_1_ (if' p_1_
          (traceProgramNextOp "parseTrace" "o_pc" (parseTrace_o_pc data_))
          (traceProgramPred "parseTrace" "p_emptyLine" p_1_1_ (if' p_1_1_
            (traceProgramPred "parseTrace" "p_eol" p_1_1_0_ (if' p_1_1_0_
              (traceProgramNextOp "parseTrace" "o_err1" (parseTrace_o_err1 data_))
              (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
            ))
            (traceProgramNextOp "parseTrace" "o_other" (parseTrace_o_other data_)) 
          )) 
        )) 
      ))

parseTrace_o_other (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_other" data_ [False,False,False,False,False,False,False,False,False,True,False] flow_)
  where
    otherLines' = (append otherLines line)
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    result' = result
    otherLines_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    result_iDn_ = result_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_firstPC data_)
    p_0_ = (parseTrace_p_eol data_)
    p_1_ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_firstPC" p__ (if' p__
        (traceProgramPred "parseTrace" "p_eol" p_0_ (if' p_0_
          (traceProgramHalt "parseTrace" data_)
          (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
        ))
        (traceProgramPred "parseTrace" "p_eol" p_1_ (if' p_1_
          (traceProgramNextOp "parseTrace" "o_err1" (parseTrace_o_err1 data_))
          (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
        )) 
      ))

parseTrace_o_pc (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_pc" data_ [False,False,False,False,False,True,False,False,False,False,True] flow_)
  where
    (fName,pName,vars) = ((read (drop 3 line))::(String,String,[(String,String)]))
    firstPC' = True
    result' = (TraceTree (initTrace fName pName vars) [])
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    firstPC_iDn_ = True
    result_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

parseTrace_o_pc_rec (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_pc_rec" data_ [True,False,False,False,False,False,False,True,True,False,True] flow_)
  where
    callCounter' = callCounter+1
    lines' = recRest
    lines2 = (prepend lines line)
    lno' = recLno
    lno2 = lno - 1
    path2 = (append curPath callCounter')
    recLno = (parseTraceLno lines2 path2 lno2)
    recRest = (parseTraceRest lines2 path2 lno2)
    recResult = (parseTrace lines2 path2 lno2)
    result' = (appendSubtree result [] recResult)
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    otherLines' = otherLines
    callCounter_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    result_iDn_ = True
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

parseTrace_o_ph (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_ph" data_ [False,False,False,False,False,False,False,False,False,False,True] flow_)
  where
    (TraceTree trace c) = result
    result' = (TraceTree (setTraceFinished trace1) c)
    tRow = (setNextOp (lastTraceRow trace) "HALT")
    trace1 = (addTraceRow (removeLastTraceRow trace) tRow)
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    result_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseTrace" data_)

parseTrace_o_pn (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_pn" data_ [False,False,False,False,False,False,False,False,False,False,True] flow_)
  where
    (TraceTree trace c) = result
    (pName,opName) = (read (drop 3 line))::(String,String)
    result' = (TraceTree trace1 c)
    tRow = (setNextOp (lastTraceRow trace) opName)
    trace1 = (addTraceRow (removeLastTraceRow trace) tRow)
    callCounter' = callCounter
    callsStored' = callsStored
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    result_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    callsStored_iDn_ = callsStored_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

parseTrace_o_po (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_po" data_ [False,True,False,False,False,False,False,False,False,False,True] flow_)
  where
    (TraceTree trace c) = result
    (pName,opName,values,reassignedList) = (read (drop 3 line))::(String,String,[String],[Bool])
    calls = (map (append curPath) [(callsStored+1) .. callCounter])
    callsStored' = callCounter
    result' = (TraceTree (addTraceRow trace tRow) c)
    tRow = (initTraceRow opName values2 reassignedList lno calls)
    values2 = (map (\x -> (if' (startsWith "u" x) "undefined" (drop 1 x))) values)
    callCounter' = callCounter
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    callsStored_iDn_ = True
    result_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

parseTrace_o_pp (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = (traceProgramOp "parseTrace" "o_pp" data_ [False,True,False,False,False,False,False,False,False,False,True] flow_)
  where
    (TraceTree trace c) = result
    (pName,predName,truth) = (read (drop 3 line))::(String,String,Bool)
    calls = (map (append curPath) [(callsStored+1) .. callCounter])
    callsStored' = callCounter
    result' = (TraceTree trace1 c)
    tRow = (addCalls (addPred (lastTraceRow trace) predName truth) calls)
    trace1 = (addTraceRow (removeLastTraceRow trace) tRow)
    callCounter' = callCounter
    curPath' = curPath
    err' = err
    errMsg' = errMsg
    firstPC' = firstPC
    line' = line
    lines' = lines
    lno' = lno
    otherLines' = otherLines
    callsStored_iDn_ = True
    result_iDn_ = True
    callCounter_iDn_ = callCounter_iD_
    curPath_iDn_ = curPath_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    firstPC_iDn_ = firstPC_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    otherLines_iDn_ = otherLines_iD_
    data_ = (Data_parseTrace callCounter' callsStored' curPath' err' errMsg' firstPC' line' lines' lno' otherLines' result' callCounter_iDn_ callsStored_iDn_ curPath_iDn_ err_iDn_ errMsg_iDn_ firstPC_iDn_ line_iDn_ lines_iDn_ lno_iDn_ otherLines_iDn_ result_iDn_)
    p__ = (parseTrace_p_eol data_)    
    flow_ = 
      (traceProgramPred "parseTrace" "p_eol" p__ (if' p__
        (traceProgramHalt "parseTrace" data_)
        (traceProgramNextOp "parseTrace" "o_next" (parseTrace_o_next data_)) 
      ))

--SHOW
instance Show Data_parseTrace where  
  show (Data_parseTrace callCounter callsStored curPath err errMsg firstPC line lines lno otherLines result callCounter_iD_ callsStored_iD_ curPath_iD_ err_iD_ errMsg_iD_ firstPC_iD_ line_iD_ lines_iD_ lno_iD_ otherLines_iD_ result_iD_) = 
    (show [
      (if' callCounter_iD_ (prepend (show callCounter) 'd') "u"),
      (if' callsStored_iD_ (prepend (show callsStored) 'd') "u"),
      (if' curPath_iD_ (prepend (show curPath) 'd') "u"),
      (if' err_iD_ (prepend (show err) 'd') "u"),
      (if' errMsg_iD_ (prepend (show errMsg) 'd') "u"),
      (if' firstPC_iD_ (prepend (show firstPC) 'd') "u"),
      (if' line_iD_ (prepend (show line) 'd') "u"),
      (if' lines_iD_ (prepend (show lines) 'd') "u"),
      (if' lno_iD_ (prepend (show lno) 'd') "u"),
      (if' otherLines_iD_ (prepend (show otherLines) 'd') "u"),
      (if' result_iD_ (prepend (show result) 'd') "u")
    ])

--END OF PROGRAM parseTrace