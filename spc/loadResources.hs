import System.IO
import System.Exit
import System.Environment
import System.Directory

import Control.Monad

import SPLib
import Program
import Trace
import Template
import Data.Map (Map)
import Data.Either
import qualified Data.Map as Map

pathSeparator = "/"

main = do
  args <- getArgs 
  if (length args)==1 then do
    let dirName = (first args)
    templates <- readTemplateFromDirectory dirName
    writeFile (dirName++".res") (show templates)
  else do
    die "only one argument allowed (directory to resources)"
  
  
readTemplateFromDirectory dir = do
    let fq x = (dir++pathSeparator++x)
    dirContents <- (listDirectory dir)  
    onlyFiles <- (filterM (doesFileExist . fq) dirContents)  
    onlyFilesContent' <- (mapM (readFile . fq) onlyFiles)
    let onlyFilesContent = (map repairScrewedEncoding onlyFilesContent')  
    let templates = (Map.fromList (zip onlyFiles onlyFilesContent))
    return templates
  
