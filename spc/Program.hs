module Program where

import Data.Map (Map)
import Data.Either
import qualified Data.Map as Map

import SPLib
import LabeledTree
import Template

data Program = Program 
  String                           --program name
  String                           --startOp
  (Map String (String,Bool))       --vars  (name -> (type,showInTrace))
  (Map String ([String],String))   --funcs (name -> (input vars, output var)  
  (Map String (Map String String)) --ops (opName -> (varName -> expr)) / varNames which do not occur in vars are helper 
  (Map String String)              --preds (name -> expr)  
  (Map String LabeledTree)         --flow (name -> expr) 
  

data ParserMode = Undef | NAME | VARS | FUNCS | OPS | PREDS | FLOW deriving (Show, Read, Eq, Enum)  
instance Show Program where
  show p = (show (toLabeledTree p)) 
 
parenthesesTree :: String -> (Maybe LabeledTree)  
countSpacesInFront :: String -> Int
removeComments :: String -> String
checkProgram :: Program -> [String]

--START OF PROGRAM parseProgram

data Data_parseProgram  = Data_parseProgram 
  Bool --assignToAdd
  Bool --cameFromNext
  Bool --err
  String --errMsg
  (Map String LabeledTree) --flow
  String --flowExpr
  String --flowName
  Bool --flowToAdd
  [String] --funcInputs
  String --funcName
  String --funcOutput
  (Map String ([String],String)) --funcs
  String --input
  Int --lastWsif
  String --line
  [String] --lines
  Int --lno
  ParserMode --mode
  (Maybe String) --name
  String --opExpr
  String --opName
  String --opVName
  (Map String (Map String String)) --ops
  String --predExpr
  String --predName
  Bool --predToAdd
  (Map String String) --preds
  Program --program
  (Maybe String) --startOp
  String --varName
  Bool --varShow
  String --varType
  (Map String (String,Bool)) --vars
  Int --wsif
  Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
parseProgram :: String -> Program
parseProgram_error :: String -> Bool
parseProgram_errorMsg :: String -> String

parseProgram_o_end1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_end2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_eop :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err10 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err11 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err12 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err13 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err14 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err16 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err17 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err18 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err19 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err20 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err21 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err22 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err4 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err5 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err6 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err7 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err8 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_err9 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_flow1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_flow2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_flow3 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_funcs1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_funcs2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_init :: Data_parseProgram -> Data_parseProgram
parseProgram_o_mode :: Data_parseProgram -> Data_parseProgram
parseProgram_o_name :: Data_parseProgram -> Data_parseProgram
parseProgram_o_next :: Data_parseProgram -> Data_parseProgram
parseProgram_o_ops1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_ops2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_ops3 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_ops4 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_ops5 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_preds1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_preds2 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_preds3 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_vars1 :: Data_parseProgram -> Data_parseProgram
parseProgram_o_vars2 :: Data_parseProgram -> Data_parseProgram

parseProgram_p_assignNotSet :: Data_parseProgram -> Bool
parseProgram_p_assignToAdd :: Data_parseProgram -> Bool
parseProgram_p_cameFromNext :: Data_parseProgram -> Bool
parseProgram_p_containsDoubleColon :: Data_parseProgram -> Bool
parseProgram_p_containsEqual :: Data_parseProgram -> Bool
parseProgram_p_emptyLine :: Data_parseProgram -> Bool
parseProgram_p_endsWithColon :: Data_parseProgram -> Bool
parseProgram_p_eop :: Data_parseProgram -> Bool
parseProgram_p_err :: Data_parseProgram -> Bool
parseProgram_p_flowNotSet :: Data_parseProgram -> Bool
parseProgram_p_flowToAdd :: Data_parseProgram -> Bool
parseProgram_p_funcNotSet :: Data_parseProgram -> Bool
parseProgram_p_legalMode :: Data_parseProgram -> Bool
parseProgram_p_modeFlow :: Data_parseProgram -> Bool
parseProgram_p_modeFuncs :: Data_parseProgram -> Bool
parseProgram_p_modeName :: Data_parseProgram -> Bool
parseProgram_p_modeOps :: Data_parseProgram -> Bool
parseProgram_p_modePreds :: Data_parseProgram -> Bool
parseProgram_p_modeVars :: Data_parseProgram -> Bool
parseProgram_p_nameIsSet :: Data_parseProgram -> Bool
parseProgram_p_opNotSet :: Data_parseProgram -> Bool
parseProgram_p_predNotSet :: Data_parseProgram -> Bool
parseProgram_p_predToAdd :: Data_parseProgram -> Bool
parseProgram_p_startOpIsSet :: Data_parseProgram -> Bool
parseProgram_p_startsWithExclamationMark :: Data_parseProgram -> Bool
parseProgram_p_startsWithHashtag :: Data_parseProgram -> Bool
parseProgram_p_varNotSet :: Data_parseProgram -> Bool
parseProgram_p_wsif :: Data_parseProgram -> Bool

--DEFINITIONS
parseProgram input = (traceProgramCall "parseProgram" "parseProgram" [("errMsg","String"),("lno","Int"),("mode","ParserMode")]  program')
  where
    (Data_parseProgram _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ program' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseProgram_o_init (Data_parseProgram undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined input undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined False False False False False False False False False False False False True False False False False False False False False False False False False False False False False False False False False False))

parseProgram_error input = (traceProgramCall "parseProgram_error" "parseProgram" [("errMsg","String"),("lno","Int"),("mode","ParserMode")]  err')
  where
    (Data_parseProgram _ _ err' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseProgram_o_init (Data_parseProgram undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined input undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined False False False False False False False False False False False False True False False False False False False False False False False False False False False False False False False False False False))

parseProgram_errorMsg input = (traceProgramCall "parseProgram_errorMsg" "parseProgram" [("errMsg","String"),("lno","Int"),("mode","ParserMode")]  errMsg')
  where
    (Data_parseProgram _ _ _ errMsg' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (parseProgram_o_init (Data_parseProgram undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined input undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined False False False False False False False False False False False False True False False False False False False False False False False False False False False False False False False False False False))

parseProgram_p_assignNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup opVName (just (Map.lookup opName ops))))

parseProgram_p_assignToAdd (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = assignToAdd

parseProgram_p_cameFromNext (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = cameFromNext

parseProgram_p_containsDoubleColon (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (not (isNothing (strPos line "::" 0)))

parseProgram_p_containsEqual (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (contains line '=')

parseProgram_p_emptyLine (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isEmpty line)

parseProgram_p_endsWithColon (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = ((last line) == ':')

parseProgram_p_eop (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isEmpty lines)

parseProgram_p_err (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = err

parseProgram_p_flowNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup flowName flow))

parseProgram_p_flowToAdd (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = flowToAdd

parseProgram_p_funcNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup funcName funcs))

parseProgram_p_legalMode (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (not (isNothing ((readMaybe (removeFirst line))::(Maybe ParserMode))))

parseProgram_p_modeFlow (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == FLOW)

parseProgram_p_modeFuncs (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == FUNCS)

parseProgram_p_modeName (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == NAME)

parseProgram_p_modeOps (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == OPS)

parseProgram_p_modePreds (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == PREDS)

parseProgram_p_modeVars (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (mode == VARS)

parseProgram_p_nameIsSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (not (isNothing name))

parseProgram_p_opNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup opName ops))

parseProgram_p_predNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup predName preds))

parseProgram_p_predToAdd (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = predToAdd

parseProgram_p_startOpIsSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (not (isNothing startOp))

parseProgram_p_startsWithExclamationMark (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = ((first line) == '!')

parseProgram_p_startsWithHashtag (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = ((first line) == '#')

parseProgram_p_varNotSet (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (isNothing (Map.lookup varName vars))

parseProgram_p_wsif (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (wsif > lastWsif)

parseProgram_o_end1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_end1" data_ [False,False,False] flow_)
  where
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_nameIsSet data_)
    p_0_ = (parseProgram_p_startOpIsSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_nameIsSet" p__ (if' p__
        (traceProgramPred "parseProgram" "p_startOpIsSet" p_0_ (if' p_0_
          (traceProgramNextOp "parseProgram" "o_end2" (parseProgram_o_end2 data_))
          (traceProgramNextOp "parseProgram" "o_err2" (parseProgram_o_err2 data_)) 
        ))
        (traceProgramNextOp "parseProgram" "o_err1" (parseProgram_o_err1 data_)) 
      ))

parseProgram_o_end2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_end2" data_ [False,False,False] flow_)
  where
    program' = (Program (just name) (just startOp) vars funcs ops preds flow)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    program_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_eop (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_eop" data_ [False,False,False] flow_)
  where
    cameFromNext' = False
    assignToAdd' = assignToAdd
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    cameFromNext_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_eop data_)
    p_0_ = (parseProgram_p_predToAdd data_)
    p_0_1_ = (parseProgram_p_assignToAdd data_)
    p_0_1_1_ = (parseProgram_p_flowToAdd data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_eop" p__ (if' p__
        (traceProgramPred "parseProgram" "p_predToAdd" p_0_ (if' p_0_
          (traceProgramNextOp "parseProgram" "o_preds3" (parseProgram_o_preds3 data_))
          (traceProgramPred "parseProgram" "p_assignToAdd" p_0_1_ (if' p_0_1_
            (traceProgramNextOp "parseProgram" "o_ops5" (parseProgram_o_ops5 data_))
            (traceProgramPred "parseProgram" "p_flowToAdd" p_0_1_1_ (if' p_0_1_1_
              (traceProgramNextOp "parseProgram" "o_flow3" (parseProgram_o_flow3 data_))
              (traceProgramNextOp "parseProgram" "o_end1" (parseProgram_o_end1 data_)) 
            )) 
          )) 
        ))
        (traceProgramNextOp "parseProgram" "o_next" (parseProgram_o_next data_)) 
      ))

parseProgram_o_err1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err1" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "start op not set in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err10 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err10" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "no mode set, expected line to start with '#' in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err11 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err11" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "expected line to contain '=' in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err12 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err12" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "expected line to contain '::' in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err13 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err13" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "expected line to end with ':' / no op started yet in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err14 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err14" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "illegal mode in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err16 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err16" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "program name redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err17 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err17" data_ [True,False,False] flow_)
  where
    errMsg' = "empty function name or output variable in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err18 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err18" data_ [True,False,False] flow_)
  where
    errMsg' = "empty variable name or type in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err19 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err19" data_ [True,False,False] flow_)
  where
    errMsg' = "LHS or RHS of previous predicate is empty in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err2" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "name not set in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err20 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err20" data_ [True,False,False] flow_)
  where
    errMsg' = "empty operation name in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err21 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err21" data_ [True,False,False] flow_)
  where
    errMsg' = "LHS or RHS of previous assignment is empty in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err22 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err22" data_ [True,False,False] flow_)
  where
    errMsg' = "LHS or RHS of previous flow is empty in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err4 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err4" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "variable redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err5 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err5" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "function redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err6 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err6" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "operation redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err7 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err7" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "assignment redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err8 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err8" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "flow redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_err9 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_err9" data_ [True,False,False] flow_)
  where
    err' = True
    errMsg' = "predicate redeclared in line "++(show lno)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    errMsg_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramHalt "parseProgram" data_)

parseProgram_o_flow1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_flow1" data_ [False,False,False] flow_)
  where
    (x,y) = (splitAtFirst "=" line)
    flowExpr' = (trim y)
    flowName' = (trim x)
    flowToAdd' = True
    lastWsif' = wsif
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    flowExpr_iDn_ = True
    flowName_iDn_ = True
    flowToAdd_iDn_ = True
    lastWsif_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_flowNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_flowNotSet" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))
        (traceProgramNextOp "parseProgram" "o_err8" (parseProgram_o_err8 data_)) 
      ))

parseProgram_o_flow2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_flow2" data_ [False,False,False] flow_)
  where
    flowExpr' = (flowExpr ++ " " ++ line)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    flowExpr_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_flow3 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_flow3" data_ [False,False,False] flow_)
  where
    err' = (if' ((isEmpty flowName) || (isEmpty flowExpr)) True err)
    flow' = (Map.insert flowName (just (parenthesesTree flowExpr)) flow)
    flowToAdd' = False
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flowExpr' = flowExpr
    flowName' = flowName
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    flow_iDn_ = True
    flowToAdd_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_cameFromNext data_)
    p_1_0_ = (parseProgram_p_containsEqual data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err22" (parseProgram_o_err22 data_))
        (traceProgramPred "parseProgram" "p_cameFromNext" p_1_ (if' p_1_
          (traceProgramPred "parseProgram" "p_containsEqual" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "parseProgram" "o_flow1" (parseProgram_o_flow1 data_))
            (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
          ))
          (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_)) 
        )) 
      ))

parseProgram_o_funcs1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_funcs1" data_ [False,False,False] flow_)
  where
    (x,y) = (splitAtFirst "=" line)
    err' = (if' ((isEmpty funcName') || (isEmpty funcOutput')) True err)
    funcInputs' = (if' (isEmpty z) [] (removeFirst z))
    funcName' = (if' (isEmpty z) "" (first z))
    funcOutput' = (trim y)
    z = (filter (not.isEmpty) (map trim (split " " (trim x))))
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    funcInputs_iDn_ = True
    funcName_iDn_ = True
    funcOutput_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_funcNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err17" (parseProgram_o_err17 data_))
        (traceProgramPred "parseProgram" "p_funcNotSet" p_1_ (if' p_1_
          (traceProgramNextOp "parseProgram" "o_funcs2" (parseProgram_o_funcs2 data_))
          (traceProgramNextOp "parseProgram" "o_err5" (parseProgram_o_err5 data_)) 
        )) 
      ))

parseProgram_o_funcs2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_funcs2" data_ [False,False,False] flow_)
  where
    funcs' = (Map.insert funcName (funcInputs,funcOutput) funcs)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    funcs_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_init (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_init" data_ [False,True,True] flow_)
  where
    assignToAdd' = False
    err' = False
    flow' = Map.empty
    flowToAdd' = False
    funcs' = Map.empty
    lines' = (split "\n" input)
    lno' = 0
    mode' = Undef
    name' = Nothing
    ops' = Map.empty
    predToAdd' = False
    preds' = Map.empty
    startOp' = Nothing
    vars' = Map.empty
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flowExpr' = flowExpr
    flowName' = flowName
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    input' = input
    lastWsif' = lastWsif
    line' = line
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    predExpr' = predExpr
    predName' = predName
    program' = program
    varName' = varName
    varShow' = varShow
    varType' = varType
    wsif' = wsif
    assignToAdd_iDn_ = True
    err_iDn_ = True
    flow_iDn_ = True
    flowToAdd_iDn_ = True
    funcs_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    mode_iDn_ = True
    name_iDn_ = True
    ops_iDn_ = True
    predToAdd_iDn_ = True
    preds_iDn_ = True
    startOp_iDn_ = True
    vars_iDn_ = True
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    program_iDn_ = program_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_next" (parseProgram_o_next data_))

parseProgram_o_mode (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_mode" data_ [False,False,True] flow_)
  where
    mode' = ((read (removeFirst line))::ParserMode)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    mode_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_name (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_name" data_ [False,False,False] flow_)
  where
    name' = (Just line)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    name_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_next (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_next" data_ [False,True,False] flow_)
  where
    cameFromNext' = True
    line' = (trc (first lines))
    lines' = (removeFirst lines)
    lno' = lno + 1
    wsif' = (countSpacesInFront (first lines))
    assignToAdd' = assignToAdd
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    cameFromNext_iDn_ = True
    line_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    wsif_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_emptyLine data_)
    p_1_ = (parseProgram_p_startsWithHashtag data_)
    p_1_0_ = (parseProgram_p_legalMode data_)
    p_1_1_ = (parseProgram_p_modeName data_)
    p_1_1_0_ = (parseProgram_p_nameIsSet data_)
    p_1_1_1_ = (parseProgram_p_modeFuncs data_)
    p_1_1_1_0_ = (parseProgram_p_containsEqual data_)
    p_1_1_1_1_ = (parseProgram_p_modeVars data_)
    p_1_1_1_1_0_ = (parseProgram_p_containsDoubleColon data_)
    p_1_1_1_1_1_ = (parseProgram_p_modePreds data_)
    p_1_1_1_1_1_0_ = (parseProgram_p_predToAdd data_)
    p_1_1_1_1_1_0_0_ = (parseProgram_p_wsif data_)
    p_1_1_1_1_1_0_1_ = (parseProgram_p_containsEqual data_)
    p_1_1_1_1_1_1_ = (parseProgram_p_modeFlow data_)
    p_1_1_1_1_1_1_0_ = (parseProgram_p_flowToAdd data_)
    p_1_1_1_1_1_1_0_0_ = (parseProgram_p_wsif data_)
    p_1_1_1_1_1_1_0_1_ = (parseProgram_p_containsEqual data_)
    p_1_1_1_1_1_1_1_ = (parseProgram_p_modeOps data_)
    p_1_1_1_1_1_1_1_0_ = (parseProgram_p_startOpIsSet data_)
    p_1_1_1_1_1_1_1_0_0_ = (parseProgram_p_endsWithColon data_)
    p_1_1_1_1_1_1_1_0_0_0_ = (parseProgram_p_assignToAdd data_)
    p_1_1_1_1_1_1_1_0_0_1_ = (parseProgram_p_assignToAdd data_)
    p_1_1_1_1_1_1_1_0_0_1_0_ = (parseProgram_p_wsif data_)
    p_1_1_1_1_1_1_1_0_0_1_1_ = (parseProgram_p_containsEqual data_)
    p_1_1_1_1_1_1_1_0_1_ = (parseProgram_p_endsWithColon data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_emptyLine" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))
        (traceProgramPred "parseProgram" "p_startsWithHashtag" p_1_ (if' p_1_
          (traceProgramPred "parseProgram" "p_legalMode" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "parseProgram" "o_mode" (parseProgram_o_mode data_))
            (traceProgramNextOp "parseProgram" "o_err14" (parseProgram_o_err14 data_)) 
          ))
          (traceProgramPred "parseProgram" "p_modeName" p_1_1_ (if' p_1_1_
            (traceProgramPred "parseProgram" "p_nameIsSet" p_1_1_0_ (if' p_1_1_0_
              (traceProgramNextOp "parseProgram" "o_err16" (parseProgram_o_err16 data_))
              (traceProgramNextOp "parseProgram" "o_name" (parseProgram_o_name data_)) 
            ))
            (traceProgramPred "parseProgram" "p_modeFuncs" p_1_1_1_ (if' p_1_1_1_
              (traceProgramPred "parseProgram" "p_containsEqual" p_1_1_1_0_ (if' p_1_1_1_0_
                (traceProgramNextOp "parseProgram" "o_funcs1" (parseProgram_o_funcs1 data_))
                (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
              ))
              (traceProgramPred "parseProgram" "p_modeVars" p_1_1_1_1_ (if' p_1_1_1_1_
                (traceProgramPred "parseProgram" "p_containsDoubleColon" p_1_1_1_1_0_ (if' p_1_1_1_1_0_
                  (traceProgramNextOp "parseProgram" "o_vars1" (parseProgram_o_vars1 data_))
                  (traceProgramNextOp "parseProgram" "o_err12" (parseProgram_o_err12 data_)) 
                ))
                (traceProgramPred "parseProgram" "p_modePreds" p_1_1_1_1_1_ (if' p_1_1_1_1_1_
                  (traceProgramPred "parseProgram" "p_predToAdd" p_1_1_1_1_1_0_ (if' p_1_1_1_1_1_0_
                    (traceProgramPred "parseProgram" "p_wsif" p_1_1_1_1_1_0_0_ (if' p_1_1_1_1_1_0_0_
                      (traceProgramNextOp "parseProgram" "o_preds2" (parseProgram_o_preds2 data_))
                      (traceProgramNextOp "parseProgram" "o_preds3" (parseProgram_o_preds3 data_)) 
                    ))
                    (traceProgramPred "parseProgram" "p_containsEqual" p_1_1_1_1_1_0_1_ (if' p_1_1_1_1_1_0_1_
                      (traceProgramNextOp "parseProgram" "o_preds1" (parseProgram_o_preds1 data_))
                      (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
                    )) 
                  ))
                  (traceProgramPred "parseProgram" "p_modeFlow" p_1_1_1_1_1_1_ (if' p_1_1_1_1_1_1_
                    (traceProgramPred "parseProgram" "p_flowToAdd" p_1_1_1_1_1_1_0_ (if' p_1_1_1_1_1_1_0_
                      (traceProgramPred "parseProgram" "p_wsif" p_1_1_1_1_1_1_0_0_ (if' p_1_1_1_1_1_1_0_0_
                        (traceProgramNextOp "parseProgram" "o_flow2" (parseProgram_o_flow2 data_))
                        (traceProgramNextOp "parseProgram" "o_flow3" (parseProgram_o_flow3 data_)) 
                      ))
                      (traceProgramPred "parseProgram" "p_containsEqual" p_1_1_1_1_1_1_0_1_ (if' p_1_1_1_1_1_1_0_1_
                        (traceProgramNextOp "parseProgram" "o_flow1" (parseProgram_o_flow1 data_))
                        (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
                      )) 
                    ))
                    (traceProgramPred "parseProgram" "p_modeOps" p_1_1_1_1_1_1_1_ (if' p_1_1_1_1_1_1_1_
                      (traceProgramPred "parseProgram" "p_startOpIsSet" p_1_1_1_1_1_1_1_0_ (if' p_1_1_1_1_1_1_1_0_
                        (traceProgramPred "parseProgram" "p_endsWithColon" p_1_1_1_1_1_1_1_0_0_ (if' p_1_1_1_1_1_1_1_0_0_
                          (traceProgramPred "parseProgram" "p_assignToAdd" p_1_1_1_1_1_1_1_0_0_0_ (if' p_1_1_1_1_1_1_1_0_0_0_
                            (traceProgramNextOp "parseProgram" "o_ops5" (parseProgram_o_ops5 data_))
                            (traceProgramNextOp "parseProgram" "o_ops1" (parseProgram_o_ops1 data_)) 
                          ))
                          (traceProgramPred "parseProgram" "p_assignToAdd" p_1_1_1_1_1_1_1_0_0_1_ (if' p_1_1_1_1_1_1_1_0_0_1_
                            (traceProgramPred "parseProgram" "p_wsif" p_1_1_1_1_1_1_1_0_0_1_0_ (if' p_1_1_1_1_1_1_1_0_0_1_0_
                              (traceProgramNextOp "parseProgram" "o_ops4" (parseProgram_o_ops4 data_))
                              (traceProgramNextOp "parseProgram" "o_ops5" (parseProgram_o_ops5 data_)) 
                            ))
                            (traceProgramPred "parseProgram" "p_containsEqual" p_1_1_1_1_1_1_1_0_0_1_1_ (if' p_1_1_1_1_1_1_1_0_0_1_1_
                              (traceProgramNextOp "parseProgram" "o_ops3" (parseProgram_o_ops3 data_))
                              (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
                            )) 
                          )) 
                        ))
                        (traceProgramPred "parseProgram" "p_endsWithColon" p_1_1_1_1_1_1_1_0_1_ (if' p_1_1_1_1_1_1_1_0_1_
                          (traceProgramNextOp "parseProgram" "o_ops1" (parseProgram_o_ops1 data_))
                          (traceProgramNextOp "parseProgram" "o_err13" (parseProgram_o_err13 data_)) 
                        )) 
                      ))
                      (traceProgramNextOp "parseProgram" "o_err10" (parseProgram_o_err10 data_)) 
                    )) 
                  )) 
                )) 
              )) 
            )) 
          )) 
        )) 
      ))

parseProgram_o_ops1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_ops1" data_ [False,False,False] flow_)
  where
    err' = (if' (isEmpty opName') True err)
    opName' = (removeLast line)
    startOp' = (if' (isNothing startOp) (Just opName') startOp)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    opName_iDn_ = True
    startOp_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_opNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err20" (parseProgram_o_err20 data_))
        (traceProgramPred "parseProgram" "p_opNotSet" p_1_ (if' p_1_
          (traceProgramNextOp "parseProgram" "o_ops2" (parseProgram_o_ops2 data_))
          (traceProgramNextOp "parseProgram" "o_err6" (parseProgram_o_err6 data_)) 
        )) 
      ))

parseProgram_o_ops2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_ops2" data_ [False,False,False] flow_)
  where
    ops' = (Map.insert opName Map.empty ops)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    ops_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_ops3 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_ops3" data_ [False,False,False] flow_)
  where
    (x,y) = (splitAtFirst "=" line)
    assignToAdd' = True
    lastWsif' = wsif
    opExpr' = (trim y)
    opVName' = (trim x)
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opName' = opName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    assignToAdd_iDn_ = True
    lastWsif_iDn_ = True
    opExpr_iDn_ = True
    opVName_iDn_ = True
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opName_iDn_ = opName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_assignNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_assignNotSet" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))
        (traceProgramNextOp "parseProgram" "o_err7" (parseProgram_o_err7 data_)) 
      ))

parseProgram_o_ops4 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_ops4" data_ [False,False,False] flow_)
  where
    opExpr' = (opExpr ++ "\n    " ++ (take wsif (repeat ' ')) ++ line)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    opExpr_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_ops5 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_ops5" data_ [False,False,False] flow_)
  where
    assignToAdd' = False
    err' = (if' ((isEmpty opVName) || (isEmpty opExpr)) True err)
    opEntry = (just (Map.lookup opName ops))
    ops' = (Map.insert opName (Map.insert opVName opExpr opEntry) ops)
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    assignToAdd_iDn_ = True
    err_iDn_ = True
    ops_iDn_ = True
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_cameFromNext data_)
    p_1_0_ = (parseProgram_p_endsWithColon data_)
    p_1_0_1_ = (parseProgram_p_containsEqual data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err21" (parseProgram_o_err21 data_))
        (traceProgramPred "parseProgram" "p_cameFromNext" p_1_ (if' p_1_
          (traceProgramPred "parseProgram" "p_endsWithColon" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "parseProgram" "o_ops1" (parseProgram_o_ops1 data_))
            (traceProgramPred "parseProgram" "p_containsEqual" p_1_0_1_ (if' p_1_0_1_
              (traceProgramNextOp "parseProgram" "o_ops3" (parseProgram_o_ops3 data_))
              (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
            )) 
          ))
          (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_)) 
        )) 
      ))

parseProgram_o_preds1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_preds1" data_ [False,False,False] flow_)
  where
    (x,y) = (splitAtFirst "=" line)
    lastWsif' = wsif
    predExpr' = (trim y)
    predName' = (trim x)
    predToAdd' = True
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    lastWsif_iDn_ = True
    predExpr_iDn_ = True
    predName_iDn_ = True
    predToAdd_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_predNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_predNotSet" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))
        (traceProgramNextOp "parseProgram" "o_err9" (parseProgram_o_err9 data_)) 
      ))

parseProgram_o_preds2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_preds2" data_ [False,False,False] flow_)
  where
    predExpr' = (predExpr ++ "\n" ++ (take wsif (repeat ' ')) ++ line)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    predExpr_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

parseProgram_o_preds3 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_preds3" data_ [False,False,False] flow_)
  where
    err' = (if' ((isEmpty predName) || (isEmpty predExpr)) True err)
    predToAdd' = False
    preds' = (Map.insert predName predExpr preds)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    predToAdd_iDn_ = True
    preds_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_cameFromNext data_)
    p_1_0_ = (parseProgram_p_containsEqual data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err19" (parseProgram_o_err19 data_))
        (traceProgramPred "parseProgram" "p_cameFromNext" p_1_ (if' p_1_
          (traceProgramPred "parseProgram" "p_containsEqual" p_1_0_ (if' p_1_0_
            (traceProgramNextOp "parseProgram" "o_preds1" (parseProgram_o_preds1 data_))
            (traceProgramNextOp "parseProgram" "o_err11" (parseProgram_o_err11 data_)) 
          ))
          (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_)) 
        )) 
      ))

parseProgram_o_vars1 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_vars1" data_ [False,False,False] flow_)
  where
    (x,y) = (splitAtFirst "::" line)
    err' = (if' ((isEmpty varName') || (isEmpty varType')) True err)
    varName' = (trim (if' ((not (isEmpty x)) && ((first x)=='!')) (removeFirst x) x))
    varShow' = (if' (isEmpty x) True (not ((first x)=='!')))
    varType' = (trim y)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    vars' = vars
    wsif' = wsif
    err_iDn_ = True
    varName_iDn_ = True
    varShow_iDn_ = True
    varType_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    vars_iDn_ = vars_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    p__ = (parseProgram_p_err data_)
    p_1_ = (parseProgram_p_varNotSet data_)    
    flow_ = 
      (traceProgramPred "parseProgram" "p_err" p__ (if' p__
        (traceProgramNextOp "parseProgram" "o_err18" (parseProgram_o_err18 data_))
        (traceProgramPred "parseProgram" "p_varNotSet" p_1_ (if' p_1_
          (traceProgramNextOp "parseProgram" "o_vars2" (parseProgram_o_vars2 data_))
          (traceProgramNextOp "parseProgram" "o_err4" (parseProgram_o_err4 data_)) 
        )) 
      ))

parseProgram_o_vars2 (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = (traceProgramOp "parseProgram" "o_vars2" data_ [False,False,False] flow_)
  where
    vars' = (Map.insert varName (varType,varShow) vars)
    assignToAdd' = assignToAdd
    cameFromNext' = cameFromNext
    err' = err
    errMsg' = errMsg
    flow' = flow
    flowExpr' = flowExpr
    flowName' = flowName
    flowToAdd' = flowToAdd
    funcInputs' = funcInputs
    funcName' = funcName
    funcOutput' = funcOutput
    funcs' = funcs
    input' = input
    lastWsif' = lastWsif
    line' = line
    lines' = lines
    lno' = lno
    mode' = mode
    name' = name
    opExpr' = opExpr
    opName' = opName
    opVName' = opVName
    ops' = ops
    predExpr' = predExpr
    predName' = predName
    predToAdd' = predToAdd
    preds' = preds
    program' = program
    startOp' = startOp
    varName' = varName
    varShow' = varShow
    varType' = varType
    wsif' = wsif
    vars_iDn_ = True
    assignToAdd_iDn_ = assignToAdd_iD_
    cameFromNext_iDn_ = cameFromNext_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    flow_iDn_ = flow_iD_
    flowExpr_iDn_ = flowExpr_iD_
    flowName_iDn_ = flowName_iD_
    flowToAdd_iDn_ = flowToAdd_iD_
    funcInputs_iDn_ = funcInputs_iD_
    funcName_iDn_ = funcName_iD_
    funcOutput_iDn_ = funcOutput_iD_
    funcs_iDn_ = funcs_iD_
    input_iDn_ = input_iD_
    lastWsif_iDn_ = lastWsif_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    mode_iDn_ = mode_iD_
    name_iDn_ = name_iD_
    opExpr_iDn_ = opExpr_iD_
    opName_iDn_ = opName_iD_
    opVName_iDn_ = opVName_iD_
    ops_iDn_ = ops_iD_
    predExpr_iDn_ = predExpr_iD_
    predName_iDn_ = predName_iD_
    predToAdd_iDn_ = predToAdd_iD_
    preds_iDn_ = preds_iD_
    program_iDn_ = program_iD_
    startOp_iDn_ = startOp_iD_
    varName_iDn_ = varName_iD_
    varShow_iDn_ = varShow_iD_
    varType_iDn_ = varType_iD_
    wsif_iDn_ = wsif_iD_
    data_ = (Data_parseProgram assignToAdd' cameFromNext' err' errMsg' flow' flowExpr' flowName' flowToAdd' funcInputs' funcName' funcOutput' funcs' input' lastWsif' line' lines' lno' mode' name' opExpr' opName' opVName' ops' predExpr' predName' predToAdd' preds' program' startOp' varName' varShow' varType' vars' wsif' assignToAdd_iDn_ cameFromNext_iDn_ err_iDn_ errMsg_iDn_ flow_iDn_ flowExpr_iDn_ flowName_iDn_ flowToAdd_iDn_ funcInputs_iDn_ funcName_iDn_ funcOutput_iDn_ funcs_iDn_ input_iDn_ lastWsif_iDn_ line_iDn_ lines_iDn_ lno_iDn_ mode_iDn_ name_iDn_ opExpr_iDn_ opName_iDn_ opVName_iDn_ ops_iDn_ predExpr_iDn_ predName_iDn_ predToAdd_iDn_ preds_iDn_ program_iDn_ startOp_iDn_ varName_iDn_ varShow_iDn_ varType_iDn_ vars_iDn_ wsif_iDn_)
    
    flow_ = 
      (traceProgramNextOp "parseProgram" "o_eop" (parseProgram_o_eop data_))

--SHOW
instance Show Data_parseProgram where  
  show (Data_parseProgram assignToAdd cameFromNext err errMsg flow flowExpr flowName flowToAdd funcInputs funcName funcOutput funcs input lastWsif line lines lno mode name opExpr opName opVName ops predExpr predName predToAdd preds program startOp varName varShow varType vars wsif assignToAdd_iD_ cameFromNext_iD_ err_iD_ errMsg_iD_ flow_iD_ flowExpr_iD_ flowName_iD_ flowToAdd_iD_ funcInputs_iD_ funcName_iD_ funcOutput_iD_ funcs_iD_ input_iD_ lastWsif_iD_ line_iD_ lines_iD_ lno_iD_ mode_iD_ name_iD_ opExpr_iD_ opName_iD_ opVName_iD_ ops_iD_ predExpr_iD_ predName_iD_ predToAdd_iD_ preds_iD_ program_iD_ startOp_iD_ varName_iD_ varShow_iD_ varType_iD_ vars_iD_ wsif_iD_) = 
    (show [
      (if' errMsg_iD_ (prepend (show errMsg) 'd') "u"),
      (if' lno_iD_ (prepend (show lno) 'd') "u"),
      (if' mode_iD_ (prepend (show mode) 'd') "u")
    ])

--END OF PROGRAM parseProgram

--START OF PROGRAM disassembleHs

data Data_disassembleHs  = Data_disassembleHs 
  String --const_endTag
  String --const_startTag
  Bool --err
  String --errMsg
  String --input
  String --line
  [String] --lines
  Int --lno
  [(Either (Int,Int) String)] --res
  Bool Bool Bool Bool Bool Bool Bool Bool Bool
  
--SIGNATURES
disassembleHs :: String -> [(Either (Int,Int) String)]
disassembleHs_error :: String -> Bool
disassembleHs_errorMsg :: String -> String

disassembleHs_o_err :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_init :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_next1 :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_next2 :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_regular1 :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_regular2 :: Data_disassembleHs -> Data_disassembleHs
disassembleHs_o_startTag :: Data_disassembleHs -> Data_disassembleHs

disassembleHs_p_endTag :: Data_disassembleHs -> Bool
disassembleHs_p_eol :: Data_disassembleHs -> Bool
disassembleHs_p_resEmpty :: Data_disassembleHs -> Bool
disassembleHs_p_resLastRight :: Data_disassembleHs -> Bool
disassembleHs_p_startTag :: Data_disassembleHs -> Bool

--DEFINITIONS
disassembleHs input = (traceProgramCall "disassembleHs" "disassembleHs" [("lno","Int")]  res')
  where
    (Data_disassembleHs _ _ _ _ _ _ _ _ res' _ _ _ _ _ _ _ _ _) = (disassembleHs_o_init (Data_disassembleHs undefined undefined undefined undefined input undefined undefined undefined undefined False False False False True False False False False))

disassembleHs_error input = (traceProgramCall "disassembleHs_error" "disassembleHs" [("lno","Int")]  err')
  where
    (Data_disassembleHs _ _ err' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (disassembleHs_o_init (Data_disassembleHs undefined undefined undefined undefined input undefined undefined undefined undefined False False False False True False False False False))

disassembleHs_errorMsg input = (traceProgramCall "disassembleHs_errorMsg" "disassembleHs" [("lno","Int")]  errMsg')
  where
    (Data_disassembleHs _ _ _ errMsg' _ _ _ _ _ _ _ _ _ _ _ _ _ _) = (disassembleHs_o_init (Data_disassembleHs undefined undefined undefined undefined input undefined undefined undefined undefined False False False False True False False False False))

disassembleHs_p_endTag (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res _ _ _ _ _ _ _ _ _) = ((trim line) == (const_endTag ++ (fromRight undefined (last res))))

disassembleHs_p_eol (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res _ _ _ _ _ _ _ _ _) = (isEmpty lines)

disassembleHs_p_resEmpty (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res _ _ _ _ _ _ _ _ _) = (isEmpty res)

disassembleHs_p_resLastRight (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res _ _ _ _ _ _ _ _ _) = (isRight (last res))

disassembleHs_p_startTag (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res _ _ _ _ _ _ _ _ _) = (startsWith const_startTag line)

disassembleHs_o_err (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_err" data_ [False] flow_)
  where
    err' = True
    errMsg' = ("end of program tag for program '"++(fromRight undefined (last res))++"' missing")
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    res' = res
    err_iDn_ = True
    errMsg_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    res_iDn_ = res_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "disassembleHs" data_)

disassembleHs_o_init (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_init" data_ [True] flow_)
  where
    const_endTag' = "--END OF PROGRAM "
    const_startTag' = "--START OF PROGRAM "
    err' = False
    lines' = (split "\n" input)
    lno' = 0
    res' = []
    errMsg' = errMsg
    input' = input
    line' = line
    const_endTag_iDn_ = True
    const_startTag_iDn_ = True
    err_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    res_iDn_ = True
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "disassembleHs" "o_next1" (disassembleHs_o_next1 data_))

disassembleHs_o_next1 (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_next1" data_ [True] flow_)
  where
    line' = (first lines)
    lines' = (removeFirst lines)
    lno' = lno + 1
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    err' = err
    errMsg' = errMsg
    input' = input
    res' = res
    line_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    res_iDn_ = res_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    p__ = (disassembleHs_p_startTag data_)
    p_1_ = (disassembleHs_p_resEmpty data_)
    p_1_1_ = (disassembleHs_p_resLastRight data_)    
    flow_ = 
      (traceProgramPred "disassembleHs" "p_startTag" p__ (if' p__
        (traceProgramNextOp "disassembleHs" "o_startTag" (disassembleHs_o_startTag data_))
        (traceProgramPred "disassembleHs" "p_resEmpty" p_1_ (if' p_1_
          (traceProgramNextOp "disassembleHs" "o_regular1" (disassembleHs_o_regular1 data_))
          (traceProgramPred "disassembleHs" "p_resLastRight" p_1_1_ (if' p_1_1_
            (traceProgramNextOp "disassembleHs" "o_regular1" (disassembleHs_o_regular1 data_))
            (traceProgramNextOp "disassembleHs" "o_regular2" (disassembleHs_o_regular2 data_)) 
          )) 
        )) 
      ))

disassembleHs_o_next2 (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_next2" data_ [True] flow_)
  where
    line' = (first lines)
    lines' = (removeFirst lines)
    lno' = lno + 1
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    err' = err
    errMsg' = errMsg
    input' = input
    res' = res
    line_iDn_ = True
    lines_iDn_ = True
    lno_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    res_iDn_ = res_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    p__ = (disassembleHs_p_endTag data_)
    p_0_ = (disassembleHs_p_eol data_)
    p_1_ = (disassembleHs_p_eol data_)    
    flow_ = 
      (traceProgramPred "disassembleHs" "p_endTag" p__ (if' p__
        (traceProgramPred "disassembleHs" "p_eol" p_0_ (if' p_0_
          (traceProgramHalt "disassembleHs" data_)
          (traceProgramNextOp "disassembleHs" "o_next1" (disassembleHs_o_next1 data_)) 
        ))
        (traceProgramPred "disassembleHs" "p_eol" p_1_ (if' p_1_
          (traceProgramNextOp "disassembleHs" "o_err" (disassembleHs_o_err data_))
          (traceProgramNextOp "disassembleHs" "o_next2" (disassembleHs_o_next2 data_)) 
        )) 
      ))

disassembleHs_o_regular1 (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_regular1" data_ [False] flow_)
  where
    res' = (append res (Left (lno,lno)))
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    err' = err
    errMsg' = errMsg
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    res_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    p__ = (disassembleHs_p_eol data_)    
    flow_ = 
      (traceProgramPred "disassembleHs" "p_eol" p__ (if' p__
        (traceProgramHalt "disassembleHs" data_)
        (traceProgramNextOp "disassembleHs" "o_next1" (disassembleHs_o_next1 data_)) 
      ))

disassembleHs_o_regular2 (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_regular2" data_ [False] flow_)
  where
    (x,y) = (fromLeft undefined (last res))
    res' = (append (removeLast res) (Left (x,y+1)))
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    err' = err
    errMsg' = errMsg
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    res_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    p__ = (disassembleHs_p_eol data_)    
    flow_ = 
      (traceProgramPred "disassembleHs" "p_eol" p__ (if' p__
        (traceProgramHalt "disassembleHs" data_)
        (traceProgramNextOp "disassembleHs" "o_next1" (disassembleHs_o_next1 data_)) 
      ))

disassembleHs_o_startTag (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = (traceProgramOp "disassembleHs" "o_startTag" data_ [False] flow_)
  where
    pName = (trim (drop (length const_startTag) line))
    res' = (append res (Right pName))
    const_endTag' = const_endTag
    const_startTag' = const_startTag
    err' = err
    errMsg' = errMsg
    input' = input
    line' = line
    lines' = lines
    lno' = lno
    res_iDn_ = True
    const_endTag_iDn_ = const_endTag_iD_
    const_startTag_iDn_ = const_startTag_iD_
    err_iDn_ = err_iD_
    errMsg_iDn_ = errMsg_iD_
    input_iDn_ = input_iD_
    line_iDn_ = line_iD_
    lines_iDn_ = lines_iD_
    lno_iDn_ = lno_iD_
    data_ = (Data_disassembleHs const_endTag' const_startTag' err' errMsg' input' line' lines' lno' res' const_endTag_iDn_ const_startTag_iDn_ err_iDn_ errMsg_iDn_ input_iDn_ line_iDn_ lines_iDn_ lno_iDn_ res_iDn_)
    p__ = (disassembleHs_p_eol data_)    
    flow_ = 
      (traceProgramPred "disassembleHs" "p_eol" p__ (if' p__
        (traceProgramNextOp "disassembleHs" "o_err" (disassembleHs_o_err data_))
        (traceProgramNextOp "disassembleHs" "o_next2" (disassembleHs_o_next2 data_)) 
      ))

--SHOW
instance Show Data_disassembleHs where  
  show (Data_disassembleHs const_endTag const_startTag err errMsg input line lines lno res const_endTag_iD_ const_startTag_iD_ err_iD_ errMsg_iD_ input_iD_ line_iD_ lines_iD_ lno_iD_ res_iD_) = 
    (show [
      (if' lno_iD_ (prepend (show lno) 'd') "u")
    ])

--END OF PROGRAM disassembleHs

reassembleHs :: String -> [(Either (Int,Int) String)] -> (Map String String) -> String
reassembleHs contents dis pMap = (join "\n" (map f dis))
  where
    lines = (split "\n" contents)
    f (Left (x,y))  = (join "\n" (take (y-x+1) (drop (x-1) lines)))--take those lines from contents
    f (Right pName) = (just (Map.lookup pName pMap))

countSpacesInFront x = 
  (if' (isEmpty x) 
    0
    (if' (isSpace cur)
      (1 + (countSpacesInFront x'))
      0
    )
  )
  where
    cur = (first x)
    x' = (removeFirst x)

 
removeComments x = (take (removeComments_h x False 0) x)

removeComments_h x inString i = --returns position of first comment -- or length of x
  (if' ((length x) < 2) 
    (i + (length x)) --no more comment can be found because x is too short
    (if' inString
      (if' ((cur1=='\\') && (cur2=='"')) 
        (removeComments_h (removeFirst x') True (i'+1)) --escape seq "\\"" found, skip it
        (if' (cur1=='"')
          (removeComments_h x' False i') --string ends          
          (removeComments_h x' True i') --string continues
        )
      )
      (if' ((cur1=='-') && (cur2=='-')) 
        i --comment starts
        (if' (cur1=='"')          
          (removeComments_h x' True i') --string starts
          (removeComments_h x' False i') 
        )
      )
    )
  )    
  where
    cur1 = (first x)
    cur2 = (first x')
    x' = (removeFirst x)
    i' = (i+1)

trc = trim . removeComments

parenthesesTree x = 
  (if' (not balanced) Nothing
    (if' (not enclosed) (if' containsSpaces Nothing (Just (LabeledTree x [])))
      (if' (isEmpty x2) Nothing 
        (if' containsSpaces
          res  
          (Just (LabeledTree x2 []))
        )
      )
    )    
  )
  where 
    x1 = (trim x)   
    balanced = (isBalanced x1)
    enclosed = (((first x1) == '(') && ((last x1) == ')'))    
    
    x2 = (trim (removeLast (removeFirst x1)))    
    containsSpaces = (reduce (||) (map isSpace x2))
    
    (rLabel,rest) = (splitAtFirst " " x2)        
    children = (pt_collectSubtrees (trim rest) [])
    res = (Just (LabeledTree rLabel children))

    
pt_collectSubtrees str acc =  
  (if' (isEmpty str) acc
    (if' ((first str)=='(')
      (pt_collectSubtrees (trim rest1) (append acc subtree))
      (if' (contains str ' ')
        (pt_collectSubtrees (trim rest2) (append acc (LabeledTree label [])))
        (append acc (LabeledTree str []))
      )
    )
  )
  where     
    (x,rest1) = (splitAt ((nextClosingP str 0)+1) str) -- x1 = "(asd)" , y1 = " x y (z)"
    subtree = (just (parenthesesTree x))
    (label,rest2) = (splitAtFirst " " str)       
    
nextClosingP :: String -> Int -> Int -- "abc( () d)()" , 3 = 9 (position of second ')')

nextClosingP x i = (nextClosingP_h x (i+1) 0)

nextClosingP_h x i acc =
  (if' (cur == ')')
    (if' (acc == 0) i
      (nextClosingP_h x i' (acc - 1))
    ) 
    (if' (cur == '(')
      (nextClosingP_h x i' (acc+1))
      (nextClosingP_h x i' acc)
    )
  )
  where
    cur = (get x i)    
    i' = i + 1
    acc' = acc - 1
    
isBalanced :: String -> Bool
isBalanced x = (isBalanced_h x 0)
    
isBalanced_h x acc = 
  (if' (isEmpty x) (acc == 0)
    (if' (cur == '(')
      (isBalanced_h x' (acc+1))
      (if' (cur == ')')
        (if' ((acc-1)>=0)
          (isBalanced_h x' (acc-1))
          False
        )
        (isBalanced_h x' acc)
      )
    )
  )
  where
    cur = (first x)
    x' = (removeFirst x)


instance ToLabeledTree Program where
  toLabeledTree (Program name startOp vars funcs ops preds flow)  = (LabeledTree (tlt_label p) [nameT,startOpT,varsT,funcsT,opsT,predsT,flowT])
    where
      nameT = (LabeledTree "Name" [(LabeledTree name [])])    
      startOpT = (LabeledTree "StartOperation" [(LabeledTree startOp [])]) 
      
      varsT = (toLabeledTreeNamed varsL (map (\(x,(y,z)) -> (x,y,z)) (Map.toList vars)))      
      varsL = (LabeledTree "Variables"  [(LabeledTree "Variable" [(LabeledTree "Name" []), (LabeledTree "Type" []), (LabeledTree "Show" [])])])
      --vars' = (toLabeledTreeNamed varsL (map (\(x,(y,z)) -> (x,y,z)) (toList vars)))
      
      funcsT = (toLabeledTreeNamed funcsL (map (\(x,(y,z)) -> (x,(map (\str -> (LabeledTree str [])) y),z)) (Map.toList funcs)))      
      funcsL = 
        (LabeledTree "Functions" [ 
          (LabeledTree "Function" [
            (LabeledTree "Name" []),
            (LabeledTree "Arguments" [
              (LabeledTree "Argument" [])
            ]),
            (LabeledTree "Output" [])
          ])
        ])
        
      opsT = (toLabeledTreeNamed opsL (map f (Map.toList ops)))
        where
          f (x,y) = (x, (Map.toList y))
      opsL = 
        (LabeledTree "Operations" [ 
          (LabeledTree "Operation" [
            (LabeledTree "Name" []),
            (LabeledTree "Assignments" [
              (LabeledTree "Assignment" [
                (LabeledTree "Variable" []),
                (LabeledTree "Expression" [])
              ])
            ])            
          ])
        ])      
      
      predsT = (toLabeledTreeNamed predsL (Map.toList preds))
      predsL = 
        (LabeledTree "Predicates" [ 
          (LabeledTree "Predicate" [
            (LabeledTree "Name" []),
            (LabeledTree "Expression" [])            
          ])
        ])            
      
      flowT = (toLabeledTreeNamed flowL (Map.toList flow))
      flowL = 
        (LabeledTree "Flow" [ 
          (LabeledTree "Operation" [
            (LabeledTree "Name" []),
            (LabeledTree "BDT" [])      
          ])
        ])     
      
      p = (Program name startOp vars funcs ops preds flow)
    
  toLabeledTreeNamed l p = (setLabel (toLabeledTree p) [] (rootLabel l))
  
  listToLabeledTree pList = (LabeledTree (tlt_listLabel (first pList)) (map toLabeledTree pList))
  listToLabeledTreeNamed l pList = (LabeledTree l1 (map (toLabeledTreeNamed l2) pList))
    where
      l1 = (rootLabel l)
      l2 = (LabeledTree (getLabel l [0]) [])
      
  tlt_label _ = "Program"
  tlt_listLabel _ = "[Program]"   

{-
funcs: 
 +* each func uses only variables that were defined 
ops: 
 +* every defined op has a flow
 +* each var ending with "'" -> is a defined var 
 +* each var not ending with "'" in varList -> maybe forgot '
 +* no op named HALT
flow:  
 +* every inner node has two children,   (isBinaryTree)   
 +* every inner node is a defined predicate
 +* every leaf is a defined operation or HALT
 +* every flow names a valid operation
 
Warnings:
-unused predicate  
-operation which never occurs as leaf 
 
-}

--returns [String] with list of errors; if empty list then everything is ok
checkProgram program = 
  (if' (contains opList "HALT") 
    (prepend res "Operation named 'HALT' is not allowed")
    res
  )
  where 
    varList = (map fst (Map.toList vars))
    opList = (map fst (Map.toList ops)) 
    predList = (map fst (Map.toList preds)) 
    flowList = (map fst (Map.toList flow)) 
    
    res = (reduce (++) [x1,x2,x3,x4,x5,x6,x7,x8])  
  
    x1_1 = -- [(func,[vars])]
      (map 
        (\(x,(y,z)) -> (x,(append y z)))
        (Map.toList funcs)
      )   
    x1_2 = --[(func,var)] flattened
      (reduce (++)
        (map 
          (\(x,y) -> (map (\z -> (x,z)) y)) 
          x1_1
        )
      )    
    x1_3 = (filter (\(x,z) -> (not (contains varList z))) x1_2)
    x1 = (map (\(x,z) -> ("Function '"++x++"' contains undeclard variable '"++z++"'")) x1_3) 
        
    x2_1 = (filter (\op -> (not (contains flowList op))) opList)      
    x2 = (map (\x -> ("Operation '"++x++"' has no flow")) x2_1)
    (Program name startOp vars funcs ops preds flow) = program

    x3_1 =  --list (opname,varname)
      (reduce (++)
        (map 
          (\(opName,assigns) -> 
            (map 
              (\(varName,expr) -> (opName,varName))
              (Map.toList assigns)
            )
          ) 
          (Map.toList ops)
        )
      )
    x3_1a = --ends with ', remove it 
      (map
        (\(opName,varName) -> (opName,(removeLast varName)))
        (filter (\(_,varName) -> ((last varName)=='\'')) x3_1)
      )
    x3_1b = --does not end with '
      (filter (\(_,varName) -> ((last varName)/='\'')) x3_1)
    x3 = 
      (map 
        (\(opName,varName) -> "Variable '"++varName++"\'' in operation '"++opName++"' undeclared")
        (filter (\(opName,varName) -> (not (contains varList varName))) x3_1a) 
      )
    x4 = 
      (map 
        (\(opName,varName) -> "Variable '"++varName++"' in operation '"++opName++"' forgot apostroph at the end?")
        (filter (\(opName,varName) -> (contains varList varName)) x3_1b) 
      )  

    x5_1 = (map fst (filter (\(x,y) -> (not (isBinaryTree y))) (Map.toList flow)))          
    x5 = (map (\x -> "Flow '"++x++"' is not a binary tree")  x5_1)
    
    x6_1 = (map (\(x,y) -> (x,y,(allNodes y)) ) (Map.toList flow))
    x6_2 = (reduce (++) (map (\(x,y,z) -> (map (\node -> (x,y,node)) z)) x6_1))
    x6_2a = (filter (\(x,y,z) -> ((rootDegree (subtree y z))>0)) x6_2)  -- inner nodes
    x6_2b = (filter (\(x,y,z) -> ((rootDegree (subtree y z))==0)) x6_2) -- leaves
    
    x6_3 = (filter (\(x,y,z) -> (not (contains predList (getLabel y z)))) x6_2a)
        
    x6 = (map (\(x,y,z) -> "Inner node '"++(show z)++"' ('"++(getLabel y z)++"') of flow '"++x++"' is not a declared predicate")  x6_3)
    
    x7_1 = (filter (\(x,y,z) -> (not (contains (prepend opList "HALT") (getLabel y z)))) x6_2b)
    x7 = (map (\(x,y,z) -> "Leaf node "++(show z)++" ('"++(getLabel y z)++"') of flow '"++x++"' is not a declared operation or HALT")  x7_1)
    
    x8_1 = (filter (\x -> (not (contains opList x))) flowList)
    x8 = (map (\x -> "Flow '"++x++"' not declared as operation") x8_1)
    
isBinaryTree t = 
  (if' ((rootDegree t)==0) 
    True 
    (((rootDegree t)==2) && (isBinaryTree (subtree t [0])) && (isBinaryTree (subtree t [0])))
  )


convertFlow :: String -> Int -> Path -> LabeledTree -> LabeledTree --first argument is program name, second indent

convertFlow pName indent path (LabeledTree l []) = res
  where    
    ws = (take indent (repeat ' '))
    res = 
      (if' (l=="HALT") 
        (LabeledTree "data" [(makeRecord "Halt" "True"),(makeRecord "ProgramName" pName),(makeRecord "Indent" ws)])
        (LabeledTree "data" [(makeRecord "Operation" l),(makeRecord "ProgramName" pName),(makeRecord "Indent" ws)])
      )
    
convertFlow pName indent path (LabeledTree l c) = res
  where 
    ws = (take indent (repeat ' '))  
    res = 
      (LabeledTree "data" [
        (makeRecord "Predicate" l),
        (makeRecord "Path" (join "_" (map show path))),
        (makeRecord "ProgramName" pName),
        (makeRecord "HasChildren" "True"),
        (makeRecord "Indent" ws),
        (makeDataSet "Flow" c' )])
    c' = (map f2 (zip c [0 .. ((length c)-1)]) )
    f2 (childSubtree,pathNo) = (convertFlow pName (indent + 2) (append path pathNo) childSubtree)
    
--first argument is format
programToTemplateData :: Program -> LabeledTree
programToTemplateData program = res
  where 
    (Program name startOp vars funcs ops preds flow) = program
    res = 
      (addDataSet "Predicates" ds_predicates
      (addDataSet "Operations" ds_operations
      (addDataSet "Functions" ds_functions
      (addDataSet "VariablesShow" ds_variables_show
      (addDataSet "Variables" ds_variables      
      (addRecord "StartOp" startOp 
      (addRecord "ProgramName" name 
        (LabeledTree "data" []))))))))

    ds_variables_show = (map f vs)
      where
        vs = (filter (snd.snd) (Map.toList vars))
        f (name,_) = 
          (addRecord "VariableName" name          
            (LabeledTree "data" []))
        
    ds_variables = (map f (Map.toList vars))
      where
        f (name,(typ,sh)) = 
          (addRecord "VariableName" name
          (addRecord "VariableType" typ
          --(addRecord "VariableShow" (show sh)
            (LabeledTree "data" [])))--)
            
    ds_functions = (map f (Map.toList funcs))
      where
        f (fname,(inputs,output)) =
          (addDataSet "InputList" (ds_functions_inputList inputs)
          (addDataSet "OutputList" (ds_functions_outputList output)
          (addDataSet "FunctionArguments" (ds_functions_arguments inputs)
          (addRecord "VariablesShow" vs
          (addRecord "ProgramName" name
          (addRecord "StartOp" startOp
          (addRecord "FunctionOutput" output
          (addRecord "FunctionOutputType" (fst (just (Map.lookup output vars)))
          (addRecord "FunctionName" fname
            (LabeledTree "data" []))))))))))   
        vs = (show (map f1 (filter (snd.snd) (Map.toList vars))))
          where
            f1 (x,(y,z)) = (x,y)

    ds_functions_inputList inputs = (map f (Map.toList vars))
      where
        f (varName,_) = 
          (if' (contains inputs varName)
            (addRecord "VariableName" varName (LabeledTree "data" []))
            (LabeledTree "data" [])
          )
            
    ds_functions_outputList output = (map f (Map.toList vars))
      where
        f (varName,_) = 
          (if' (varName == output)
            (addRecord "VariableName" output (LabeledTree "data" []))
            (LabeledTree "data" [])
          )
    
    ds_functions_arguments inputs = (map f inputs)
      where
        f varName =         
          (addRecord "ArgumentName" varName
          (addRecord "ArgumentType" (fst (just (Map.lookup varName vars)))
            (LabeledTree "data" [])))  

    ds_operations = (map f (Map.toList ops))
      where
        f (opName,aMap) =       
          (addDataSet "Flow" (ds_flow opName)
          (addDataSet "Assignments" (ds_assignments aMap)
          (addDataSet "Predicates" (ds_op_preds opName)
          (addDataSet "Variables" ds_variables        
          (addRecord "ReassignedList" reassignedList
          (addRecord "ProgramName" name
          (addRecord "OpName" opName
            (LabeledTree "data" []))))))))
          where
            reassignedList = (join "," (map show rl))
            vs = (map fst (filter (snd.snd) (Map.toList vars))) --variables names to show 
            --rl = (map (\varName -> True) vs)
            rl = (map (\varName -> f1 varName)  vs) --map to False iff assignment in curIp var' = var
            f1 varName = (not (isNothing (Map.lookup (append varName '\'') (just (Map.lookup opName ops)))))
    
    ds_op_preds opName = res      --assignments of the form p_0_1_2_ = (MySort_p_lt data_)   
      where
        res = 
          (if' (isNothing bdtM) 
            (error ("no flow found for: "++opName))
            res2 
          )
        bdtM = (Map.lookup opName flow) --is LabeledTree 
        bdt = (just bdtM)
        nodes = (filter ((/=0) . (rootDegree) . (subtree bdt)) (allNodes bdt))
        labels = (map (getLabel bdt) nodes)
        res2 = (map f2 (zip nodes labels))
        f2 (node,label) = (LabeledTree "data" [
            (makeRecord "ProgramName" name),
            (makeRecord "Path" (join "_" (map show node))),
            (makeRecord "Predicate" label)
          ])
    
    ds_flow opName = [res]
      where
        res = 
          (if' (isNothing (Map.lookup opName flow)) 
            (error ("no flow found for: "++opName))
            (convertFlow name 6 [] (just (Map.lookup opName flow)))
          )

    ds_predicates = (map f (Map.toList preds))
      where
        f (predName,predExpr) =       
          (addDataSet "Variables" ds_variables
          (addRecord "ProgramName" name
          (addRecord "PredExpr" predExpr
          (addRecord "PredName" predName
            (LabeledTree "data" [])))))
            
    ds_assignments aMap = (map f aList)
      where
        f (varName,expr) = 
          (addRecord "Expr" expr
          (addRecord "VarName" varName
            (LabeledTree "data" [])))        
        aList = (Map.toList aMap) ++ (map g untouched) ++ (map g1 touched) ++ (map g2 untouched)
          where
            g (varName,_) =  ((append varName '\''), varName)
            g1 (varName,_) = (varName++"_iDn_","True")
            g2 (varName,_) = (varName++"_iDn_",varName++"_iD_")
        untouched = (filter g (Map.toList vars))
          where
            g (varName,_) = (isNothing (Map.lookup (append varName '\'') aMap))
        touched = (filter g (Map.toList vars))
          where
            g (varName,_) = (not (isNothing (Map.lookup (append varName '\'') aMap)))            


--translateProgram :: Program -> String
{-
translateProgram (Program name startOp vars funcs ops preds flow) = tDataMain --(template templates tData "body")  
  where
    tDataMain = (addRecord empty "name" name)
    --(LabeledTree "data" [(record "name" name)])    
    
    tDataSetVars = (makeDataSet )
    -}

    
  
--testStr = "#NAME \nbubbleSort\n\n#FUNCS\nsort list = list\n\n#VARS \nlist :: [Int]\ncur :: Int\npass :: Int\nsorted :: Bool\n\n#PREDS\np_ls2  = ((length list) >= 2)\np_swap = ((list!!cur) > (list!!(cur+1))) \np_eor =  ((cur + pass + 1) == (length list)) --end of row\np_next = ((not sorted) && ((pass + 2) < (length list)))\n\n#OPS\no_init:  \n  cur' = 0\n  pass' = 0\n  sorted' = True\n  \no_move:\n  cur' = cur + 1\n\no_swap:\n  list' = (swapListElements list cur (cur+1))\n  cur' = cur + 1\n  sorted' = False\n\no_next:\n  cur' = 0\n  pass' = pass + 1\n  sorted' = True\n  \n#FLOW\no_init = (p_ls2 (p_swap o_swap o_move) HALT)\no_move = (p_eor (p_next o_next HALT) (p_swap o_swap o_move))\no_swap = (p_eor (p_next o_next HALT) (p_swap o_swap o_move))\no_next = (p_swap o_swap o_move)\n"

--p1 = (conditionProgramForTemplateData (parseProgram testStr))