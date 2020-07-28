import System.IO
import System.Exit
import System.Environment
import System.Directory

import Control.Monad

import Resources

import SPLib
import Program
import Trace
import Template
import Data.Map (Map)
import Data.Either
import qualified Data.Map as Map

pathSeparator = "/"
csvSeparatorDefault = ";"

helpMsg = SPLib.join "\n"
  [ 
    "Usage:",
    "",
    "spc fileName.hs",
    "  All sp-programs referenced in fileName.hs are looked",
    "  up in the directory fileName and translated into", 
    "  Haskell code and then inserted into fileName.hs.",
    "",
    "spc fileName.sp",
    "  fileName.sp is translated into Haskell code (fileName.hs)",
    "  and its internal representation (fileName.lt).",
    "",
    "spc fileName.tr",
    "  the trace fileName.tr is translated into csv-files",
    "  which are placed in the directory fileName.",
    "",
    "spc fileName.tr csvSep",
    "  same as above but csvSep is used as separator in",
    "  the csv-files instead of ';'.",
    ""    
  ]


main = do
  args <- getArgs 
  if (length args)==1 then do
    let fName = (first args)
    main2 fName csvSeparatorDefault
  else do
    if (length args)==2 then do
      let fName = (first args)
      let csvSeparator = (last args)
      main2 fName csvSeparator
    else do
      putStrLn helpMsg
  
main2 fName csvSeparator = do
    --read templates from directory instead from Resources.hs
    --templatesHs <- readTemplateFromDirectory "tmpl"
    --templatesCsv <- readTemplateFromDirectory "tmpl_csv"
    let compileSp program = (template templatesHs (programToTemplateData program) "body.hs")
    let compileCsv trace other = (template templatesCsv (traceToTemplateData csvSeparator trace other) "body.csv")    
    if (endsWith ".sp" fName) then do
      program <- loadSpFile fName
      let hsSnippet = compileSp program
      let fName2 = (dropAtEnd (length ".sp") fName) --remove .sp at the end 
      writeFile (fName2++".lt") (show program)      
      writeFile (fName2++".hs") hsSnippet
    else do 
      if (endsWith ".hs" fName) then do
        compileHsFile compileSp fName                
      else do      
        if (endsWith ".tr" fName) then do
          traceToCsv compileCsv fName
        else do
          die "unknown file ending (known: *.hs, *.sp, *.tr)" --case .lt  
    
traceToCsv compileCsv fileName = do  
  let baseFileName = (dropAtEnd (length ".tr") fileName)
  exists <- doesFileExist fileName      
  if exists then do    
    contents <- readFile fileName
    createDirectoryIfMissing False baseFileName    
    let lines = (split "\n" contents)
    traceToCsv2 compileCsv baseFileName lines 0 0
  else do
    die ("File '"++fileName++"' does not exist") 

traceToCsv2 compileCsv baseFileName lines i lno = do  
  let fileNameCsv i path = baseFileName++pathSeparator++(SPLib.join "-" (map (show.(+1)) (prepend path i)))++".csv" 
  let traceTree = parseTrace          lines [(i+1)] lno
  let rest      = parseTraceRest      lines [(i+1)] lno
  let other     = parseTraceOther     lines [(i+1)] lno
  let lno2      = parseTraceLno       lines [(i+1)] lno
  let err       = parseTrace_error    lines [(i+1)] lno
  let errMsg    = parseTrace_errorMsg lines [(i+1)] lno
  
  if err then do
    if (i==0) then do
      die ("Error parsing '"++baseFileName++".tr': "++errMsg)
    else
      return () 
  else do
    --convert traces to csv files
    let paths = (Trace.allNodes traceTree)
    let traces = (map (Trace.getLabel traceTree) paths) --combine with other only the root one 
    let others = (prepend (take ((length traces)-1) (repeat [])) other)
    let csvOut = (map (\(trace,other) -> compileCsv trace other) (zip traces others))
    let fNames = (map (fileNameCsv i) paths)
    (mapM (\(fn,c) -> writeFile fn c) (zip fNames csvOut))
    
    if (isEmpty rest) then do
      return ()
    else do
      traceToCsv2 compileCsv baseFileName rest (i+1) lno2     
        
compileHsFile compileSp fileName = do
  let baseFileName = (dropAtEnd (length ".hs") fileName)
  exists <- doesFileExist fileName  
  if exists then do
      contents <- readFile fileName    --split in lines \n ; change input for disassembleHs   
      let dis = (disassembleHs contents)                 
      let disErr = (disassembleHs_error contents)
      let disErrMsg = (disassembleHs_errorMsg contents)
      if disErr then do
        die ("Error while disassembling file '"++fileName++"': "++disErrMsg)
      else do
        let programNames = (map (fromRight undefined) (filter isRight dis))
        let programPaths = (map (\pName -> (baseFileName++"/"++pName++".sp")) programNames)  
        programs <- (mapM loadSpFile programPaths)
        let programs2 = (map compileSp programs)
        let pMap = (Map.fromList (zip programNames programs2))
        let result = (reassembleHs contents dis pMap) --map Left (x,y) <- take lines x,y and 
        writeFile (baseFileName++".hs") result
  else do
    die ("File '"++fileName++"' does not exist")

    
loadSpFile fileName = do 
  exists <- doesFileExist fileName
  if exists then do
    contents <- readFile fileName
    let parseError = parseProgram_error contents
    let parseMsg   = parseProgram_errorMsg contents
    let program    = parseProgram contents
    if parseError then do
      die ("Parse error in file '"++fileName++"':\n"++parseMsg)
    else do 
      let checks = checkProgram program
      if (isEmpty checks) then do
        return program
      else
        die ("Semantic error(s) in file '"++fileName++"':\n"++(SPLib.join "\n" checks))
  else do
    die ("File '"++fileName++"' does not exist")
  
  
readTemplateFromDirectory dir = do
    let fq x = (dir++pathSeparator++x)
    dirContents <- (listDirectory dir)  
    onlyFiles <- (filterM (doesFileExist . fq) dirContents)  
    onlyFilesContent' <- (mapM (readFile . fq) onlyFiles)
    let onlyFilesContent = (map repairScrewedEncoding onlyFilesContent')  
    let templates = (Map.fromList (zip onlyFiles onlyFilesContent))
    return templates
  
