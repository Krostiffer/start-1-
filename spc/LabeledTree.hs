module LabeledTree where

--import Debug.Trace

import SPLib

--todo: FromLabeledTree instances, automatic ToLabeledTree instance

data LabeledTree = LabeledTree String [LabeledTree] -- (Eq,Show,Read)
type Path = [Int] -- [] = root node, [0,2] = first child of root node, then third child of that node

rootDegree :: LabeledTree -> Int
rootLabel :: LabeledTree -> String

getLabel :: LabeledTree -> Path -> String 
setLabel :: LabeledTree -> Path -> String -> LabeledTree
degree   :: LabeledTree -> Path -> Int
 
subtree :: LabeledTree -> Path -> LabeledTree 
pathExists :: LabeledTree -> Path -> Bool 

appendSubtree :: LabeledTree -> Path -> LabeledTree -> LabeledTree
removeSubtree :: LabeledTree -> Path -> Int -> LabeledTree
insertSubtree :: LabeledTree -> Path -> Int -> LabeledTree -> LabeledTree
setSubtree    :: LabeledTree -> Path -> Int -> LabeledTree -> LabeledTree

findLabelPath :: LabeledTree -> [String] -> (Maybe Path)
findPathByKey :: LabeledTree -> [String] -> Int -> [String] -> (Maybe Path)

allNodes :: LabeledTree -> [Path] --returns list of all nodes referenced by path
size :: LabeledTree -> Int

--parenthesesTree :: String -> (Maybe LabeledTree) -- (a (b c d)) converted ; separator " ", 

--stringListToLabeledTree :: [String] -> LabeledTree -- ["a","b","c","d"] -> (a (b (c (d))))

--list of labels should be LabeledTree instead of [String] for Tuple and Map and other non-unary type functions
--helper function [String] -> path of labeled trees (first element is root label) 

class FromLabeledTree a where 
  fromLabeledTree :: LabeledTree -> a
  fromLabeledTreeNamed :: LabeledTree -> LabeledTree -> a --first arg contains the names 
  
class ToLabeledTree a where
  toLabeledTree           :: a -> LabeledTree
  toLabeledTreeNamed      :: LabeledTree -> a -> LabeledTree   --first arg contains the names
  listToLabeledTree       :: [a] -> LabeledTree 
  listToLabeledTreeNamed  :: LabeledTree -> [a] -> LabeledTree --first arg contains the names
  tlt_label               :: a -> String
  tlt_listLabel           :: a -> String 

-- DEFINITIONS
instance Eq LabeledTree where
  (LabeledTree l1 c1) == (LabeledTree l2 c2) = 
    (if' ((l1 == l2) && ((length c1) == (length c2))) 
      (if' ((length c1) == 0) True (reduce (&&) x))      
      False
    )
    where
      x = (map (\(x,y) -> (x==y)) (zip c1 c2))

instance Show LabeledTree where
  show tree = (showLabeledTree tree 0)

showLabeledTree :: LabeledTree -> Int -> String
showLabeledTree (LabeledTree label children) depth = (join "\n" res)
    where 
      rootStr = (take depth (repeat '\t')) ++ (escape label) 
      childrenStr = (map (\x -> (showLabeledTree x (depth+1))) children)
      res = (prepend childrenStr rootStr) 

instance Read LabeledTree where  
  readsPrec _ = readLabeledTree

readLabeledTree :: String -> [(LabeledTree, String)]
readLabeledTree str = 
  (if' ((fst (first lines))>0) [] --(error "root node has leading tab(s)")
    (if' ((length lines) == 1)    
      [(tree,"")]
      (if' (isNothing recTreeM) [] [((just recTreeM),"")])
    )
  )
  where
    tree = (LabeledTree (unescape (snd (first lines))) [])  
    recTreeM = (readLabeledTree_h tree [] (removeFirst lines))
    lines = (map f (split "\n" str))
    f line = (i,(drop i line)) -- "\t\tabc" -> (2,"abc")
      where
        i = (just (find pred (reverse [0..(length line)]))) 
        pred i = (((take i (repeat '\t'))++(drop i line))==line)

readLabeledTree_h tree curNode lines = --(trace (show (d,l,curNode,tree))  --(trace (show tree)  --(trace (show (line,d,l,aNode)) 
    (if' (d==0) Nothing --(error "non-root node has 0 leading tabs") 
      (if' (d > ((length curNode)+1))
        Nothing --(error "node has to many leading tabs")
        (if' ((length lines)>1)
          (if' (isNothing recResM) Nothing recResM)
          (Just (appendSubtree tree aNode (LabeledTree l [])))
        )
      )
    )
    --)
    where
      line = (first lines)
      d = (fst line) --depth of next node 
      l = (unescape (snd line)) --label of next node
      aNode = (reverse (drop ((length curNode) - d + 1) (reverse curNode)))
      k = (rootDegree (subtree tree aNode)) --next node is (k+1)-th child of curNode    
      
      recResM = (readLabeledTree_h (appendSubtree tree aNode (LabeledTree l [])) (append aNode k) (removeFirst lines)) 
           
        
rootDegree (LabeledTree _ c) = (length c)
rootLabel (LabeledTree l _) = l

subtree (LabeledTree l c) p = (if' (isEmpty p) (LabeledTree l c) (subtree (get c (first p)) (removeFirst p)) ) 
getLabel (LabeledTree l c) p = (if' (isEmpty p) l (getLabel (get c (first p)) (removeFirst p)) ) 
pathExists (LabeledTree l c) p = 
  (if' (isEmpty p) 
    True 
    (if' (((first p) >= 0) && ((first p) < (length c)))
      (pathExists (get c (first p)) (removeFirst p)) 
      False    
    ) 
  )

genericModifyTree  :: (LabeledTree -> LabeledTree) -> LabeledTree -> Path -> LabeledTree
genericModifyTree f (LabeledTree l c) p = 
  (if' (isEmpty p)
      (f (LabeledTree l c))
      (LabeledTree l c')
  )
  where
    c' = (set c (first p) (genericModifyTree f (get c (first p)) (removeFirst p)))

  
setLabel tree path label = (genericModifyTree f tree path)
  where
    f (LabeledTree _ c) = (LabeledTree label c)
    
appendSubtree tree path subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (append c subtree))

removeSubtree tree path i = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (remove c i))

insertSubtree tree path i subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (insert c i subtree))
    
setSubtree tree path i subtree = (genericModifyTree f tree path)
  where
    f (LabeledTree l c) = (LabeledTree l (set c i subtree))    

--ex: (a (b c) (b d) (b x e)) [b,e] -> [2,1]   
findLabelPath tree labelPath = (findLabelPath_h tree labelPath 0)

findLabelPath_h (LabeledTree l c) lp offset = 
  (if' (isEmpty lp)
    (Just [])
    (if' (contains cl (first lp))
      (if' (isNothing x)
        (findLabelPath_h (LabeledTree l (remove c index)) lp (offset+1))
        (Just (prepend (just x) (index+offset)))
      )      
      Nothing
    )
  )
  where    
    cl = (map (\t -> (rootLabel t)) c)
    index = (indexOf cl (first lp))    
    x = (findLabelPath (get c index) (removeFirst lp))

findPathByKey tree labelPath up labelPath2 = 
  (if' (isNothing p1M) Nothing
    (if' (isNothing p2M) Nothing 
      (Just res)
    )
  )
  where
    p1M = (findLabelPath tree labelPath)
    p1 = (just p1M)
    p1up = (reverse (drop up (reverse p1)))
    p2M = (findLabelPath (subtree tree p1up) labelPath2)
    res = p1up++(just p2M)

allNodes t = (allNodes_h t [])

allNodes_h :: LabeledTree -> Path -> [Path]    
allNodes_h (LabeledTree _ c) cur = 
  (if' (isEmpty c) 
    [cur] 
    (prepend  
      (reduce (++) (map (\(i,t) -> (allNodes_h t (append cur i))) c'))
      cur
    )
  )
  where
    c' = (zip [0..((length c)-1)] c)
    
size t = (length (allNodes t))
degree t p = (rootDegree (subtree t p))
    
escapeTable = [ ('\\',"\\\\"), ('\n',"\\n"), ('\t',"\\t") ] --important: "\\" must come before the others 
escape str = (escape_h str escapeTable) 
escape_h str [] = str
escape_h str ((x,y):z) = (escape_h (join y (split [x] str)) z)

unescape str =  
  (if' (contains str '\\') 
    (if' (isEmpty y) (error "string to unescape contains trailing '\\'")
      (if' (not valid) (error "string to unescape contains invalid escape sequence")
        (x++(prepend (unescape (removeFirst y)) realChar))
      )
    ) 
    str
  )
  where    
    (x,y) = (splitAtFirst "\\" str)
    a = ['\\','n','t']
    valid = ((not (isEmpty y)) && (contains a (first y)))
    index = (indexOf a (first y))
    realChar = (fst (get escapeTable index))
    

--INSTANCE ToLabeledTree 

instance ToLabeledTree Char where    
  toLabeledTreeNamed labels x = (LabeledTree (escape (rootLabel labels)) [(LabeledTree (escape [x]) [])])  
  toLabeledTree x = (toLabeledTreeNamed (LabeledTree (tlt_label x) []) x)  
  listToLabeledTreeNamed labels xList = (LabeledTree (escape (rootLabel labels)) [(LabeledTree (escape xList) [])])
  listToLabeledTree xList = (listToLabeledTreeNamed (LabeledTree (tlt_listLabel (first xList)) []) xList) 
  tlt_label _ = "Char"
  tlt_listLabel _ = "String"
  
instance ToLabeledTree Bool where    
  toLabeledTreeNamed labels x = (LabeledTree (escape (rootLabel labels)) [(LabeledTree (show x) [])])  
  toLabeledTree x = (toLabeledTreeNamed (LabeledTree (tlt_label x) []) x)
  listToLabeledTreeNamed labels xList = (LabeledTree (escape label1) (map (toLabeledTreeNamed (LabeledTree label2 [])) xList))  
    where
      label1 = (rootLabel labels)
      label2 = (getLabel labels [0])
  listToLabeledTree xList = (listToLabeledTreeNamed labels xList)
    where 
      labels = (LabeledTree l1 [(LabeledTree l2 [])])
      l1 = (tlt_listLabel (first xList))
      l2 = (tlt_label (first xList))
  tlt_label _ = "Bool"
  tlt_listLabel _ = "[Bool]"    
  
instance ToLabeledTree Int where    
  toLabeledTreeNamed labels x = (LabeledTree (escape (rootLabel labels)) [(LabeledTree (show x) [])])  
  toLabeledTree x = (toLabeledTreeNamed (LabeledTree (tlt_label x) []) x)
  listToLabeledTreeNamed labels xList = (LabeledTree (escape label1) (map (toLabeledTreeNamed label2) xList))  
    where
      label1 = (rootLabel labels)
      label2 = (subtree labels [0])
  listToLabeledTree xList = (listToLabeledTreeNamed labels xList)
    where 
      labels = (LabeledTree l1 [(LabeledTree l2 [])])
      l1 = (tlt_listLabel (first xList))
      l2 = (tlt_label (first xList))
  tlt_label _ = "Int"
  tlt_listLabel _ = "[Int]"        

  
instance ToLabeledTree LabeledTree where    
  toLabeledTreeNamed labels x = (LabeledTree (escape (rootLabel labels)) [x])
  toLabeledTree x = (LabeledTree (tlt_label x) [x])
  listToLabeledTreeNamed labels xList = (LabeledTree (escape label1) (map (toLabeledTreeNamed label2) xList))
    where
      label1 = (rootLabel labels)
      label2 = (subtree labels [0])
  listToLabeledTree xList = (LabeledTree (tlt_listLabel (first xList)) (map toLabeledTree xList))
  tlt_label _ = "Program"
  tlt_listLabel _ = "[Program]"
 
 
instance (ToLabeledTree b) => ToLabeledTree [b] where    
  toLabeledTreeNamed labels xList = (listToLabeledTreeNamed labels xList)  
  toLabeledTree xList = (listToLabeledTree xList)    
  listToLabeledTreeNamed labels xList2 = (LabeledTree (escape label1) (map (toLabeledTreeNamed labels') xList2))  
    where
      label1 = (rootLabel labels)
      labels' = (subtree labels [0])
  listToLabeledTree xList2 = (LabeledTree label (map listToLabeledTree xList2))
    where
      label = (tlt_listLabel (first xList2))      
  tlt_label xList = (tlt_listLabel (first xList))
  tlt_listLabel xList = "["++(tlt_label xList)++"]"

--toLabeledTreeNamed (LabeledTree "A1" [(LabeledTree "A2" [(LabeledTree "A3" [])])]) [[True]]  
  
instance (ToLabeledTree a, ToLabeledTree b) => ToLabeledTree (a,b) where    
  toLabeledTreeNamed labels (x,y) = (LabeledTree label 
    [
      (toLabeledTreeNamed labels_a x),
      (toLabeledTreeNamed labels_b y)
    ])
    where
      label = (rootLabel labels)
      labels_a = (subtree labels [0])
      labels_b = (subtree labels [1])
  toLabeledTree (x,y) = (LabeledTree label 
    [
      (toLabeledTree x),
      (toLabeledTree y)
    ])
    where
      label = (tlt_label (x,y))  
  listToLabeledTreeNamed labels xList = (LabeledTree (escape label1) (map (toLabeledTreeNamed labels') xList))  
    where
      label1 = (rootLabel labels)
      labels' = (subtree labels [0])
  listToLabeledTree xList = (LabeledTree (escape label) (map toLabeledTree xList))  
    where
      label = (tlt_listLabel xList)  
  tlt_label t = "(" ++ (tlt_label (fst t)) ++ "," ++ (tlt_label (snd t)) ++ ")"
  
  tlt_listLabel x = ("["++(tlt_label x)++"]")  

  -- (x,y,z)
instance (ToLabeledTree a, ToLabeledTree b, ToLabeledTree c) => ToLabeledTree (a,b,c) where    
  toLabeledTreeNamed labels (x,y,z) = (LabeledTree label 
    [
      (toLabeledTreeNamed labels_a x),
      (toLabeledTreeNamed labels_b y),
      (toLabeledTreeNamed labels_c z)
    ])
    where
      label = (rootLabel labels)
      labels_a = (subtree labels [0])
      labels_b = (subtree labels [1])
      labels_c = (subtree labels [2])
  toLabeledTree (x,y,z) = (LabeledTree label 
    [
      (toLabeledTree x),
      (toLabeledTree y),
      (toLabeledTree z)
    ])
    where
      label = (tlt_label (x,y,z))  
  listToLabeledTreeNamed labels xList = (LabeledTree (escape label1) (map (toLabeledTreeNamed labels') xList))  
    where
      label1 = (rootLabel labels)
      labels' = (subtree labels [0])
  listToLabeledTree xList = (LabeledTree (escape label) (map toLabeledTree xList))  
    where
      label = (tlt_listLabel (first xList))  
  tlt_label t = "(" ++ (tlt_label (t31 t)) ++ "," ++ (tlt_label (t32 t)) ++ "," ++ (tlt_label (t33 t)) ++ ")"
    where
      t31 (x,_,_) = x
      t32 (_,x,_) = x
      t33 (_,_,x) = x
  tlt_listLabel x = ("["++(tlt_label x)++"]")    
  
-- 3,4,5 tuples support   
  
--for (Map k v) == [(k,v)]
  
    