module Template where

import Data.Map (Map)
import qualified Data.Map as Map

import SPLib
import LabeledTree

{-
data TemplateData = TemplateData 
  [(String,String)] --records
  [(String,[TemplateData])] --datasets 
  
  
show2 -> for all types that can be displayed as labeledtree
show2 = show.toLabeledTree 
-}  
 
--template array

template :: (Map String String) -> LabeledTree -> String -> String --args: templates indexed by name, data used, template name to evaluate

-- Guillemets/Angle Quotation Marks \171 = '«' and \187 '»'
tplStartTag = '\171'
tplEndTag   = '\187'
tplTagSeparator   = ';'

--repairScrewedEncoding = id
repairScrewedEncoding x = x2 
  where 
    tplStartTagScrewed = "\9516\189"
    tplEndTagScrewed = "\9516\9559"
    replace s r x = (join r (split s x))
    x1 = (replace tplStartTagScrewed [tplStartTag] x)
    x2 = (replace tplEndTagScrewed [tplEndTag] x1)
    
makeRecord key value = 
  (LabeledTree "record" [
    (LabeledTree "key" [(LabeledTree key [])]), 
    (LabeledTree "value" [(LabeledTree value [])])
  ])
makeDataSet name sets = 
  (LabeledTree "dataSet" [
    (LabeledTree "name" [ (LabeledTree name []) ]),
    (LabeledTree "sets" sets)
  ])

addRecord key value tdata = (appendSubtree tdata [] (makeRecord key value))
addDataSet name sets tdata = (appendSubtree tdata [] (makeDataSet name sets))
  
    
--data, record, key, value, dataSet, name sets
{-
  0: tagName
  1  tmplName (simply evaluates tmpl with given name and same tData)
  2: keyName, trueTmpl, falseTmpl
  3: dataSetName, tmpl, separator tmpl  
-}

template templates tData tName = 
  (if' (isNothing tmplM) (error errMsg)
    (template_h templates tData tName tmpl)
  )
  where 
    tmplM = (Map.lookup tName templates)
    tmpl = (just tmplM)  
    errMsg = "template named '"++tName++"' not found"

template_h templates tData tName tmpl = 
  (if' (contains tmpl tplStartTag)    
    (if' (not (hasEndingTag)) (error errMsg1)
      (if' (isNothing tagTypeM) (error errMsg2)
        (tmplInit++evalTag++(template_h templates tData tName tmplRest))
      )
    )
    tmpl --contains no tags, return verbatim tmpl 
  )
  where  
    (tmplInit,x) = (splitAtFirst [tplStartTag] tmpl)  
    (tag,tmplRest) = (splitAtFirst [tplEndTag] x)
    hasEndingTag  = (contains x tplEndTag)
    
    digitCharList = (map (\x -> (first (show x))) [0..9])
    tagTypeM = (findIndex (\x -> (x == (first tag))) digitCharList)
    tagType = (just tagTypeM)
    tag' = (removeFirst tag)
    
    evalTag = 
      (if' ((tagType < 0) || (tagType >= (length (evals)))) (error "template named '"++tName++"' uses unknown tag type '"++(show tagType)++"'")
        (get evals tagType)
      )
    evals = [eval0,eval1,eval2,eval3]    
    eval0 = --inserts value from tData with key name $tag
      (if' ((isNothing pathM) || ((rootDegree (subtree tData (just pathM)))==0)) (error errMsg)     
        val
      ) 
      where
        pathM = (findPathByKey tData ["record","key",tag'] 2 ["value"])        
        val = (getLabel tData ((just pathM)++[0]))
        errMsg = "value for key '"++tag'++"' not found in template named '"++tName++"'\n"++(show tData)
        
    eval1 = (template templates tData tag') --evaluates template with name $tag
    
    eval2 = 
      (if' ((length tagContent) /= 3) (error errMsg)
        (if' (isNothing pathM)
          tmplFalse
          tmplTrue
        )
      )
      where 
        tagContent = (split [tplTagSeparator] tag') --0=key exists, 1=truetmpl, 2=falsetmpl
        tagKey = (get tagContent 0)
        tagTrueTmplName = (get tagContent 1)
        tagFalseTmplName = (get tagContent 2)
        tmplTrue = (template templates tData tagTrueTmplName)
        tmplFalse = (template templates tData tagFalseTmplName)
        pathM = (findLabelPath tData ["record","key",tagKey])
        errMsg = "invalid tag '2"++tag++"' in template named '"++tName++"', too few arguments"
        
    eval3 = 
      (if' ((length tagContent) /= 3) (error errMsg1)
          (if' (isNothing dataPathM) (error errMsg2)
            res
          )
      )
      where
        tagContent = (split [tplTagSeparator] tag') --0=dataSet name, 1=tmpl, 2=sep tmpl
        dataSetName = (get tagContent 0)
        tmplName = (get tagContent 1)
        sepTmplName = (get tagContent 2)
        dataPathM = (findPathByKey tData ["dataSet","name",dataSetName] 2 ["sets"])
        setsSubtree = (subtree tData (just dataPathM))
        tDatas = (map (\i -> (subtree setsSubtree [(i-1)]) ) [1..(rootDegree setsSubtree)])
        dataTmpls = (map (\td -> (template templates td tmplName)) tDatas)
        sepTmpl = (template templates tData sepTmplName)
        res = (join sepTmpl dataTmpls)                    
        errMsg1 = "invalid tag '2"++tag++"' in template named '"++tName++"', too few arguments"
        errMsg2 = "data set with name '"++dataSetName++"' not found or missing child named 'sets'"
        
    errMsg1 = "template named '"++tName++"' does not close all tags"
    errMsg2 = "invalid tag type in '"++tName++"', value = '"++tag++"'"
      --(template templates tData tag) -- $tag=dataName;tmplName;tmplSepName 
