{-# LANGUAGE TypeSynonymInstances #-}

module SPLib.Tree (
  Tree(Tree), 
  LabeledTree(LabeledTree),   
  ToLabeledTree(toLabeledTree, toLabeledTreeNamed, listToLabeledTree, listToLabeledTreeNamed, tlt_labels, tlt_listLabels),
  FromLabeledTree(fromLabeledTreeM, listFromLabeledTreeM, fromLabeledTreeNamedM, listFromLabeledTreeNamedM), 
  singleTree, root, rootLabel, rootDegree, getLabel, getLabelD, getLabelP, setLabel, setLabelD, setLabelP, degree, degreeD,
  degreeP, getLeafLabel, getLeafLabelD, getLeafLabelP, 
  allNodes, size, subtree, subtreeD, subtreeP, pathExists, 
  appendSubtree, appendSubtreeD, appendSubtreeP, 
  removeSubtree, removeSubtreeD, removeSubtreeP, 
  insertSubtree, insertSubtreeD, insertSubtreeP, 
  setSubtree, setSubtreeD, setSubtreeP,
  findPath, findPathD, findPathP,  
  readLabeledTree, readLabeledTreeD, readLabeledTreeP, readTree, readTreeD, readTreeP, genericReadFromLabeledTree,
  lt2ts, escape, unescape, unescapeD, unescapeP
  ) where 

import Prelude (lookup) 
import SPLib.Basic
import SPLib.List as L
import qualified SPLib.Map as M
import SPLib.PartialFunction

data Tree a = Tree a [Tree a] deriving Eq
newtype LabeledTree = LabeledTree (Tree String) deriving Eq
type Path = [Int] 
type CallLocation = String


instance Show LabeledTree where
  show = showLabeledTree 0
  
instance Read LabeledTree where
  readsPrec _ str = if' (inDomain readLabeledTreeP str) [(readLabeledTree str,"")] []  
  
instance Show a => Show (Tree a) where
  show t = showLabeledTree 0 (LabeledTree (SPLib.Tree.map show t))
  
instance Read a => Read (Tree a) where
  readsPrec _ str = if' (inDomain readTreeP' str) [(applyUnsafe readTreeP' str,"")] [] 
    where
      readTreeP' = readTreeP :: (Read a => PartialFunction String (Tree a))

class ToLabeledTree a where
  toLabeledTree           :: a -> LabeledTree
  listToLabeledTree       :: [a] -> LabeledTree 
  toLabeledTreeNamed      :: (Tree String) -> a -> LabeledTree   --first arg contains the names    
  listToLabeledTreeNamed  :: (Tree String) -> [a] -> LabeledTree --first arg contains the names
  tlt_labels              :: a -> Tree String
  tlt_listLabels          :: a -> Tree String  
    
  listToLabeledTreeNamed labels x = LabeledTree (Tree (escape rLabel) (L.map f x))  
    where
      rLabel = rootLabel labels
      labels' = subtreeD "listToLabeledTreeNamed; forgot a label?" [0] labels
      f x = lt2ts $ toLabeledTreeNamed labels' x    
  toLabeledTree x = toLabeledTreeNamed (tlt_labels x) x
  listToLabeledTree x = listToLabeledTreeNamed (tlt_listLabels x) x
  tlt_listLabels x = (Tree rLabel [labels']) 
    where
      rLabel = "[" ++ rootLabel labels' ++ "]"
      labels' = tlt_labels x

class (ToLabeledTree a) => FromLabeledTree a where 
  fromLabeledTreeM           :: a -> LabeledTree -> Maybe a --first argument should be (undefined::a) 
  listFromLabeledTreeM       :: a -> LabeledTree -> Maybe [a]  
  fromLabeledTreeNamedM      :: (Tree String) -> LabeledTree -> Maybe a
  listFromLabeledTreeNamedM  :: (Tree String) -> LabeledTree -> Maybe [a]  
  
  fromLabeledTreeM x y = fromLabeledTreeNamedM (tlt_labels x) y
  listFromLabeledTreeM x y = listFromLabeledTreeNamedM (tlt_labels x) y
  listFromLabeledTreeNamedM labels lt =
    (if' (rLabel /= l) Nothing
      (if' (not $ isEmpty rN) Nothing
        (Just (L.map just rM))
      )
    )
    where
      (LabeledTree (Tree l c)) = lt 
      rLabel = rootLabel labels
      cLabel = getLabel [0] labels
      labels' = subtree [0] labels
      
      c' = filter (\t -> ((rootLabel t)==cLabel)) c
      rM = L.map (\t -> (fromLabeledTreeNamedM labels' (LabeledTree t))) c'
      rN = filter isNothing rM   
  
  --make either version for P and M 

singleTree :: a -> Tree a 

root       :: Path            
rootLabel  :: Tree a -> a
rootDegree :: Tree a -> Int

getLabel  :: Path -> Tree a -> a
getLabelD :: CallLocation -> Path -> Tree a -> a
getLabelP :: PartialFunction (Path, Tree a) a
setLabel  :: a -> Path -> Tree a -> Tree a
setLabelD :: CallLocation -> a -> Path -> Tree a -> Tree a
setLabelP :: PartialFunction (a, Path, Tree a) (Tree a)
degree    :: Path -> Tree a -> Int
degreeD   :: CallLocation -> Path -> Tree a -> Int
degreeP   :: PartialFunction (Path, Tree a) Int

getLeafLabel  :: Path -> Tree a -> a --gets label of the unique child of (subtree path tree) which is a leaf, used for order-invariant access
getLeafLabelD :: CallLocation -> Path -> Tree a -> a
getLeafLabelP :: PartialFunction (Path, Tree a) a 
 
allNodes :: Tree a -> [Path] --returns list of all nodes
size     :: Tree a -> Int 
 
--let t = (a (b c d) (e f)) , s = (1 (2 3) 4)
subtree    :: Path -> Tree a -> Tree a -- subtree [] t = t ; subtree [0] t = (b c d) ; subtree [1] t = (e f)
subtreeD   :: CallLocation -> Path -> Tree a -> Tree a
subtreeP   :: PartialFunction (Path, Tree a) (Tree a) 
pathExists :: Path -> Tree a -> Bool 

appendSubtree  :: Tree a -> Path -> Tree a -> Tree a --appendSubtree s [1] t = (a (b c d) (e f (1 (2 3) 4)))
appendSubtreeD :: CallLocation -> Tree a -> Path -> Tree a -> Tree a 
appendSubtreeP :: PartialFunction (Tree a, Path, Tree a) (Tree a)
removeSubtree  :: Int -> Path -> Tree a -> Tree a --removeSubtree 0 [] t =  (a (e f)) ; removeSubtree 1 [] t =  (a (b c d)) 
removeSubtreeD :: CallLocation -> Int -> Path -> Tree a -> Tree a 
removeSubtreeP :: PartialFunction (Int, Path, Tree a) (Tree a)
insertSubtree  :: Tree a -> Int -> Path -> Tree a -> Tree a --Int = position where subtree is inserted; 0 means that inserted subtree is the first
insertSubtreeD :: CallLocation -> Tree a -> Int -> Path -> Tree a -> Tree a 
insertSubtreeP :: PartialFunction (Tree a, Int, Path, Tree a) (Tree a) 
setSubtree     :: Tree a -> Int -> Path -> Tree a -> Tree a --Int = index of subtree that is replaced 
setSubtreeD    :: CallLocation -> Tree a -> Int -> Path -> Tree a -> Tree a 
setSubtreeP    :: PartialFunction (Tree a, Int, Path, Tree a) (Tree a) 

-- Show constraint only needed to show the label path in error message
findPath   :: (Eq a, Show a) => [a] -> Tree a -> Path
findPathD  :: (Eq a, Show a) => CallLocation -> [a] -> Tree a -> Path
findPathP  :: (Eq a, Show a) => PartialFunction ([a], Tree a) Path

map :: (a -> b) -> Tree a -> Tree b

readTree  :: Read a => String -> Tree a
readTreeD :: Read a => CallLocation -> String -> Tree a
readTreeP :: Read a => PartialFunction String (Tree a)
readLabeledTree  :: String -> LabeledTree
readLabeledTreeD :: CallLocation -> String -> LabeledTree
readLabeledTreeP :: PartialFunction String LabeledTree

genericReadFromLabeledTree :: (FromLabeledTree a) => a -> String -> [(a,String)] 

lt2ts :: LabeledTree -> Tree String --convert LabeledTree to Tree String
--escape  :: String -> String
unescape  :: String -> String
unescapeD :: CallLocation -> String -> String
unescapeP :: PartialFunction String String

--DEFINITIONS

singleTree x = (Tree x [])

root = []
rootLabel = getLabel root
rootDegree = degree root

getLabel = unpack2 . apply $ getLabelP
getLabelD cl = unpack2 . (apply' ("SPLib.Tree.getLabelD @ "++cl)) $ getLabelP
getLabelP = PartialFunction "SPLib.Tree.getLabelP" f f_ood f_err
  where
    f (p,t) = genericQueryTree g p t
      where
        genericQueryTree' = unpack3 . applyUnsafe $ genericQueryTreeP
        g (Tree l _) = l
    f_ood (p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]    
    
setLabel = unpack3 . apply $ setLabelP
setLabelD cl = unpack3 . (apply' ("SPLib.Tree.setLabelD @ "++cl)) $ setLabelP
setLabelP = PartialFunction "SPLib.Tree.setLabelP" f f_ood f_err
  where
    f (l,p,t) = genericModifyTree' g p t
      where 
        genericModifyTree' = unpack3 . applyUnsafe $ genericModifyTreeP
        g (Tree _ c) = (Tree l c)
    f_ood (_,p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]

degree = unpack2 . apply $ degreeP
degreeD cl = unpack2 . (apply' ("SPLib.Tree.degreeD @ "++cl)) $ degreeP
degreeP = PartialFunction "SPLib.Tree.degreeP" f f_ood f_err  
  where
    f (p,t) = genericQueryTree g p t
      where
        genericQueryTree' = unpack3 . applyUnsafe $ genericQueryTreeP
        g (Tree _ c) = length c
    f_ood (p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]       

getLeafLabel  = unpack2 . apply $ getLeafLabelP
getLeafLabelD cl = unpack2 . (apply' ("SPLib.Tree.getLeafLabelD @ "++cl)) $ getLeafLabelP
getLeafLabelP = PartialFunction "SPLib.Tree.getLeafLabelP" f f_ood f_err
  where
    f (p,t) = rootLabel . first . leafs $ (p,t)
    f_ood (p,t) = 
      (if' (pathExists p t)
        (if' (isEmpty $ leafs (p,t))          
          [(1,[(show p)])]
          (if' ((length $ leafs (p,t)) > 1)
            [(2,[(show p)])]
            []
          )                    
        )
        [(0,[(show p)])]
      )
    f_err = 
      [
        ["path does not exist (p = ",")"],
        ["subtree (path = ",") has no child which is a leaf"],
        ["subtree (path = ",") has more than one child which is a leaf"]
      ]
    leafs (p,t) = res
      where
        (Tree _ children) = subtree p t 
        res = filter ((==0) . rootDegree)  children
       
allNodes = allNodes_h root
allNodes_h :: Path -> Tree a -> [Path]
allNodes_h cur (Tree _ c) = 
  (if' (isEmpty c) 
    [cur] 
    (prepend cur (concat (L.map g c')))
  )
  where
    c' = zip [0..((length c)-1)] c
    g (i,t) = allNodes_h (append i cur) t
    
size = length . allNodes 

subtree = unpack2 . apply $ subtreeP
subtreeD cl = unpack2 . (apply' ("SPLib.Tree.subtreeD @ "++cl)) $ subtreeP
subtreeP = PartialFunction "SPLib.Tree.subtreeP" f f_ood f_err
  where
    f (p,t) = genericQueryTree g p t
      where
        genericQueryTree' = unpack3 . applyUnsafe $ genericQueryTreeP
        g = id
    f_ood (p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]

pathExists p (Tree _ c) = if' (p == root) True res
  where
    res = if' (inDomain getP (i,c)) (pathExists p' (applyUnsafe L.getP (i,c))) False  
    i  = first p
    p' = removeFirst p

appendSubtree  = unpack3 . apply $ appendSubtreeP
appendSubtreeD cl = unpack3 . (apply' ("SPLib.Tree.appendSubtreeD @ "++cl)) $ appendSubtreeP
appendSubtreeP = PartialFunction "SPLib.Tree.appendSubtreeP" f f_ood f_err
  where
    f (s,p,t) = genericModifyTree' g p t
      where 
        genericModifyTree' = unpack3 . applyUnsafe $ genericModifyTreeP
        g (Tree l c) = Tree l (append s c)
    f_ood (_,p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]

removeSubtree  = unpack3 . apply $ removeSubtreeP
removeSubtreeD cl = unpack3 . (apply' ("SPLib.Tree.removeSubtreeD @ "++cl)) $ removeSubtreeP
removeSubtreeP = PartialFunction "SPLib.Tree.removeSubtreeP" f f_ood f_err
  where
    f (i,p,t) = genericModifyTree' g p t
      where 
        genericModifyTree' = unpack3 . applyUnsafe $ genericModifyTreeP
        g (Tree l c) = Tree l (remove i c)
    f_ood (i,p,t) = 
      (if' (pathExists p t) 
        (if' (pathExists [i] (subtree p t))
          []
          [(1,[(show p),(show i)])]
        )        
        [(0,[(show p),(show i)])]
      )
    f_err = 
      [
        ["path "," does not exist (i=",")"],
        ["path "," exists but does not have child with index ",""]
      ]

insertSubtree  = unpack4 . apply $ insertSubtreeP
insertSubtreeD cl = unpack4 . (apply' ("SPLib.Tree.insertSubtreeD @ "++cl)) $ insertSubtreeP
insertSubtreeP = PartialFunction "SPLib.Tree.insertSubtreeP" f f_ood f_err
  where
    f (s,i,p,t) = genericModifyTree' g p t
      where 
        genericModifyTree' = unpack3 . applyUnsafe $ genericModifyTreeP
        g (Tree l c) = Tree l (remove i c)
    f_ood (_,i,p,t) = 
      (if' (pathExists p t)
        (if' ((i >= 0) && (i <= d))
          []
          [(1,[(show i),(show p),(show d)])]
        )
        [(0,[(show p),(show i)])]
      )
      where
        d = applyUnsafe degreeP (p,t)
    f_err = 
      [
        ["path "," does not exist (i=",")"],
        ["can't insert subtree at index "," at path ","; position should be at least 0 and at most ",""]        
      ]

setSubtree  = unpack4 . apply $ setSubtreeP
setSubtreeD cl = unpack4 . (apply' ("SPLib.Tree.setSubtreeD @ "++cl)) $ setSubtreeP
setSubtreeP = PartialFunction "SPLib.Tree.insertSubtreeP" f f_ood f_err
  where
    f (s,i,p,t) = genericModifyTree' g p t
      where 
        genericModifyTree' = unpack3 . applyUnsafe $ genericModifyTreeP
        g (Tree l c) = Tree l (applyUnsafe setP (s,i,c))
    f_ood (_,i,p,t) = 
      (if' (pathExists p t) 
        (if' (pathExists [i] (subtree p t))
          []
          [(1,[(show p),(show i)])]
        )        
        [(0,[(show p),(show i)])]
      )
    f_err = 
      [
        ["path "," does not exist (i=",")"],
        ["path "," exists but does not have child with index ",""]
      ]
      
--findPaths -- return all matching paths 
findPath = unpack2 . apply $ findPathP
findPathD cl = unpack2 . (apply' ("SPLib.Tree.findPathD @ "++cl)) $ findPathP
findPathP = PartialFunction "SPLib.Tree.findPathP" f f_ood f_err
  where
    f (lp,t) = just (findPathM lp t)
    f_ood (lp,t) = if' (isNothing (findPathM lp t)) [(0,[(show lp)])] []
    f_err = [["label path "," not found"]]

findPathM :: Eq a => [a] -> Tree a -> Maybe Path
findPathM labelPath (Tree l c) = 
  (if' (isEmpty labelPath)
    (Just [])
    (if' (isEmpty res)
      Nothing      
      (Just (prepend (fst (first res)) (snd (first res))))
    )
  )
  where  
    firstLabel = first labelPath
    childrenLabel = L.map rootLabel c
    labelPath' = removeFirst labelPath 
    
    --list of children which match first label along with their index 
    candidates = filter f (zip [0..((length c)-1)] c) 
      where
        f (_,(Tree l _)) = l == firstLabel
    
    res = (L.map just (filter isJust (L.map f candidates)))
      where
        f (i,t) = if' (isNothing res) Nothing (Just (i,(just res)))
          where
            res = findPathM labelPath' t            
        
map f (Tree l c) = (Tree (f l) (L.map (SPLib.Tree.map f) c))

genericQueryTree   :: (Tree a -> b) -> Path -> Tree a -> b
genericQueryTreeP  :: PartialFunction ((Tree a -> b), Path, Tree a) b
genericModifyTree  :: (Tree a -> Tree a) -> Path -> Tree a -> Tree a
genericModifyTreeP :: PartialFunction ((Tree a -> Tree a), Path, Tree a) (Tree a)

genericQueryTree  = unpack3 . apply $ genericQueryTreeP
genericQueryTreeP = PartialFunction "SPLib.Tree.genericQueryTreeP" f f_ood f_err 
  where
    f (g, p, (Tree l c)) = if' (p==root) (g (Tree l c)) (f (g, p', t'))
      where
        p' = removeFirst p 
        t' = L.get (first p) c
    f_ood (_,p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]

genericModifyTree  = unpack3 . apply $ genericModifyTreeP
genericModifyTreeP = PartialFunction "SPLib.Tree.genericModifyTreeP" f f_ood f_err 
  where
    f (g, p, (Tree l c)) = if' (p==root) (g (Tree l c)) (Tree l c')
      where
        c' = L.set x (first p) c
        x  = f (g, removeFirst p, L.get (first p) c)
    f_ood (_,p,t) = if' (pathExists p t) [] [(0,[(show p)])]
    f_err = [["path "," does not exist"]]

showLabeledTree :: Int -> LabeledTree -> String 
showLabeledTree depth (LabeledTree (Tree label children)) = (join "\n" res)
    where 
      rootStr = (take depth (repeat '\t')) ++ (escape label)
      childrenStrs = (L.map f children)
        where
          f t = showLabeledTree (depth+1) (LabeledTree t)
      res = prepend rootStr childrenStrs 
      
-- provisional implementations of readLabeledTree and readTree
readLabeledTree = apply readLabeledTreeP
readLabeledTreeD cl = (apply' ("SPLib.Tree.readLabeledTreeD @ "++cl)) readLabeledTreeP
readLabeledTreeP = PartialFunction "SPLib.Tree.readLabeledTreeP" f f_ood f_err
  where
    f = just . readLabeledTreeM
    f_ood str = if' (isNothing $ readLabeledTreeM str) [(0,[])] []
    f_err = [["parse error"]]

readLabeledTreeM :: String -> Maybe LabeledTree
readLabeledTreeM str = 
  (if' ((fst (first lines))>0) Nothing --(error "root node has leading tab(s)")
    (if' ((length lines) == 1)    
      (Just (LabeledTree tree))
      (if' (isNothing recTreeM) Nothing
        (Just (LabeledTree (just recTreeM)))
      )
    )
  )
  where
    tree = Tree (unescape (snd (first lines))) []
    recTreeM = readLabeledTree_h tree [] (removeFirst lines)
    lines = L.map f $ split "\n" str
      where
        f line = (i,(drop i line)) -- "\t\tabc" -> (2,"abc")
          where
            i = find pred (reverse [0..(length line)])
            pred i = ((take i (repeat '\t'))++(drop i line))==line

readLabeledTree_h :: Tree String -> Path -> [(Int,String)] -> Maybe (Tree String)
readLabeledTree_h tree curNode lines = 
    (if' (d==0) Nothing --(error "non-root node has 0 leading tabs") 
      (if' (d > ((length curNode)+1))
        Nothing --(error "node has to many leading tabs")
        (if' ((length lines)>1)
          (if' (isNothing recResM) Nothing recResM)
          (Just (appendSubtree (Tree l []) aNode tree))
        )
      )
    )
    where
      line = first lines
      d = fst line --depth of next node 
      l = unescape $ snd line --label of next node
      aNode = reverse (drop ((length curNode) - d + 1) (reverse curNode))
      k = rootDegree (subtree aNode tree) --next node is (k+1)-th child of curNode          
      recResM = readLabeledTree_h (appendSubtree (Tree l []) aNode tree) (append k aNode) (removeFirst lines)
                   
readTree = apply readTreeP
readTreeD cl = (apply' ("SPLib.Tree.readTreeD @ "++cl)) readTreeP
readTreeP = PartialFunction "SPLib.Tree.readTreeP" f f_ood f_err
  where
    f str = (right . readTreeE') str
    f_ood str = if' (isLeft (readTreeE' str)) (left (readTreeE' str))  []
    f_err = 
      [
        ["could not parse str as labeled tree; str = ",""],
        ["could not parse one of the labels; str = ",""]
      ]
    readTreeE' = readTreeE::(Read a => String -> Either [Error] (Tree a))
    
readTreeE :: Read a => String -> Either [Error] (Tree a)
readTreeE str = 
  (if' (isJust $ readLabeledTreeM str)
    (if' allLabelsValid
      (Right resTree)
      (Left [(1,[(show str)])])      
    )
    (Left [(0,[(show str)])])
  )
  where
    readP' = readP::(Read a => PartialFunction String a)
    (LabeledTree tree) = just $ readLabeledTreeM str
    bTree = SPLib.Tree.map (inDomain readP') tree
    allLabelsValid = reduce (&&) (L.map f (allNodes bTree))
      where
        f path = getLabel path bTree     
    resTree = SPLib.Tree.map (apply readP') tree

genericReadFromLabeledTree a str =
    (if' (not $ inDomain readLabeledTreeP str) []
      (if' (isNothing resM) []
        [(just resM,"")]
      )
    )
    where
      lt = readLabeledTree str
      resM = fromLabeledTreeM a lt  
      
    
lt2ts (LabeledTree t) = t

typeLabelParentheses x = 
  (if' (not $ contains ' ' x) x
    (if' ((first x)=='(' && (last x)==')') x
      ("("++x++")")
    )
  )
  
instance ToLabeledTree Char where    
  toLabeledTreeNamed     labels x = LabeledTree (Tree (rootLabel labels) [(Tree [x] [])])
  listToLabeledTreeNamed labels x = LabeledTree (Tree (rootLabel labels) [(Tree x [])])  
  tlt_labels _ = singleTree "Char"
  tlt_listLabels _ = singleTree "String"
  
instance ToLabeledTree Bool where    
  toLabeledTreeNamed labels x = LabeledTree (Tree (rootLabel labels) [(Tree (show x) [])])
  tlt_labels _ = singleTree "Bool"

instance ToLabeledTree Int where    
  toLabeledTreeNamed labels x = LabeledTree (Tree (rootLabel labels) [(Tree (show x) [])])
  tlt_labels _ = singleTree "Int"

instance ToLabeledTree a => ToLabeledTree [a] where   
  toLabeledTreeNamed = listToLabeledTreeNamed
  tlt_labels x = tlt_listLabels (first x)

instance (ToLabeledTree a) => ToLabeledTree (Maybe a) where    
  toLabeledTreeNamed labels Nothing  = LabeledTree (Tree (rootLabel labels) [(Tree "Nothing" [])])
  toLabeledTreeNamed labels (Just x) = LabeledTree (Tree (rootLabel labels) [(Tree "Just" [c])])
    where
      nothingLabel = getLabelD "toLabeledTreeNamed<Maybe>: 'Nothing' label missing" [0] labels 
      justLabel = getLabelD "toLabeledTreeNamed<Maybe>: 'Just' label missing" [1] labels 
      c = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Maybe>: subtree of 'Just' in labels missing" [1,0] labels) x
  tlt_labels x = (Tree rLabel [(Tree "Nothing" []),(Tree "Just" [labels])])
    where
      rLabel = "Maybe " ++ (typeLabelParentheses (rootLabel labels))
      labels = tlt_labels (just x) 
  
instance (ToLabeledTree a, ToLabeledTree b) => ToLabeledTree (Either a b) where    
  toLabeledTreeNamed labels (Left x) = 
    (LabeledTree 
      (Tree (rootLabel labels) [
        (Tree (getLabelD "toLabeledTreeNamed<Either>: 'Left' label missing" [0] labels) [c])
      ])
    )
    where
      c = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Either>: subtree of 'Left' in labels missing" [0,0] labels) x  
  toLabeledTreeNamed labels (Right x) = 
    (LabeledTree 
      (Tree (rootLabel labels) [
        (Tree (getLabelD "toLabeledTreeNamed<Either>: 'Right' label missing" [1] labels) [c])
      ])
    )
    where
      c = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Either>: subtree of 'Right' in labels missing" [1,0] labels) x
  tlt_labels x = (Tree rLabel [(Tree "Left" [labelsL]),(Tree "Right" [labelsR])])
    where
      rLabel = "Either " ++ (typeLabelParentheses (rootLabel labelsL)) ++ " " ++ (typeLabelParentheses (rootLabel labelsR))
      labelsL = tlt_labels (left x)
      labelsR = tlt_labels (right x)         
  
instance (ToLabeledTree k, ToLabeledTree v) => ToLabeledTree (M.Map k v) where    
  toLabeledTreeNamed labels x = toLabeledTreeNamed labels (M.toList x)  
  tlt_labels x = (Tree rLabel [labels'])
    where
      rLabel = concat ["Map ",typeLabelParentheses $ rootLabel kLabels," ",typeLabelParentheses $ rootLabel vLabels]
      labels' = subtree [0] $ tlt_labels $ M.toList x
      kLabels = tlt_labels (fst (first (M.toList x)))
      vLabels = tlt_labels (snd (first (M.toList x)))

instance (Show a, ToLabeledTree a) => ToLabeledTree (Tree a) where    
  toLabeledTreeNamed labels (Tree l c) = 
    (LabeledTree 
      (Tree rLabel [
        (Tree aLabel [
          (Tree (show l) [])
        ]),
        (Tree cLabel  children ) 
      ])
    )
    where      
      rLabel  = rootLabel labels 
      aLabel  = getLabelD "toLabeledTreeNamed<Tree>: label of a type missing" [0] labels
      cLabel  = getLabelD "toLabeledTreeNamed<Tree>: label of children missing" [1] labels 
      labels' = subtreeD  "toLabeledTreeNamed<Tree>: subtree of children missing" [1,0] labels      
      children = L.map (lt2ts . (toLabeledTreeNamed labels')) c                  
  tlt_labels x = (Tree ("Tree "++(typeLabelParentheses $ rootLabel aLabels)) [aLabels,children])
    where 
      (Tree l _) = x
      aLabels = tlt_labels l
      children = tlt_listLabels (Tree l []) 

{-instance ToLabeledTree LabeledTree where    
  toLabeledTreeNamed labels (LabeledTree tree) = toLabeledTreeNamed labels tree                  
  tlt_labels _ = z
    where 
      z = 
        (Tree "LabeledTree" [
          (Tree "Label" []),
          (Tree "Children" [z])
        ])-}

--Tuple   
instance (ToLabeledTree a1, ToLabeledTree a2) => ToLabeledTree (a1,a2) where    
  toLabeledTreeNamed labels (x1,x2) = (LabeledTree (Tree (rootLabel labels) [t1,t2]))
    where
      t1 = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Tuple2>: t1 missing" [0] labels) x1
      t2 = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Tuple2>: t2 missing" [1] labels) x2  
  tlt_labels x = (Tree rLabel c)
    where
      rLabel = "("++(join ", " (L.map rootLabel c))++")"
      c = [tlt_labels x1, tlt_labels x2]
      (x1,_) = x
      (_,x2) = x

instance (ToLabeledTree a1, ToLabeledTree a2, ToLabeledTree a3) => ToLabeledTree (a1,a2,a3) where    
  toLabeledTreeNamed labels (x1,x2,x3) = (LabeledTree (Tree (rootLabel labels) [t1,t2,t3]))
    where
      t1 = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Tuple3>: t1 missing" [0] labels) x1
      t2 = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Tuple3>: t2 missing" [1] labels) x2
      t3 = lt2ts $ toLabeledTreeNamed (subtreeD "toLabeledTreeNamed<Tuple3>: t3 missing" [2] labels) x3
  tlt_labels x = (Tree rLabel c)
    where
      rLabel = "("++(join ", " (L.map rootLabel c))++")"
      c = [tlt_labels x1, tlt_labels x2, tlt_labels x3]  
      (x1,_,_) = x
      (_,x2,_) = x      
      (_,_,x3) = x      
  

--FromLabeledTree
instance FromLabeledTree Char where
  fromLabeledTreeNamedM labels (LabeledTree tree)  =         
    (if' ((rootLabel tree) /= (rootLabel labels)) Nothing
      (if' (not $ inDomain getLeafLabelP (root,tree)) Nothing
        (if' ((length x) /= 1) Nothing
          (Just (first x))
        )            
      )
    )
    where
      x = getLeafLabel root tree
      
  listFromLabeledTreeNamedM labels (LabeledTree tree) = 
    (if' ((rootLabel tree) /= (rootLabel labels)) Nothing
      (if' (not $ inDomain getLeafLabelP (root,tree)) Nothing
        (Just (getLeafLabel root tree))
      )
    )
          
instance FromLabeledTree Int where
  fromLabeledTreeNamedM labels (LabeledTree tree) = 
    (if' ((rootLabel tree) /= (rootLabel labels)) Nothing
      (if' (not $ inDomain getLeafLabelP (root,tree)) Nothing
        (if' (not $ inDomain (readP::PartialFunction String Int) lbl) Nothing
          (Just ((read lbl)::Int))
        )            
      )
    )
    where
      lbl = getLeafLabel root tree            
      
instance FromLabeledTree Bool where
  fromLabeledTreeNamedM labels (LabeledTree tree) = 
    (if' ((rootLabel tree) /= (rootLabel labels)) Nothing
      (if' (not $ inDomain getLeafLabelP (root,tree)) Nothing
        (if' (not $ inDomain (readP::PartialFunction String Bool) lbl) Nothing
          (Just ((read lbl)::Bool))
        )            
      )
    )
    where
      lbl = getLeafLabel root tree            

instance FromLabeledTree a => FromLabeledTree [a] where
  fromLabeledTreeNamedM = listFromLabeledTreeNamedM

instance (Ord k, FromLabeledTree k, FromLabeledTree v) => FromLabeledTree (M.Map k v) where
  fromLabeledTreeNamedM labels lt = 
    (if' (isNothing resM) Nothing
      (Just (M.fromList (just resM)))
    )
    where
      resM = fromLabeledTreeNamedM labels lt

{-
 TODO: make order-invariant and ignore additional subtrees
 if Nothing label and Just label exist then return Nothing
 if more than one child of the root node has Nothing label then return Nothing (same for Just label)
-}
instance FromLabeledTree a => FromLabeledTree (Maybe a) where
  fromLabeledTreeNamedM labels (LabeledTree tree) =
    (if' ((rootLabel tree) /= rLabel) Nothing
      (if' (not $ pathExists [0] tree) Nothing
        (if' (l2 == nLabel) (Just Nothing)
          (if' (l2 /= jLabel) Nothing
            (if' (not $ pathExists [0,0] tree) Nothing
              (if' (isNothing resM) Nothing
                (Just resM)
              )
            )
          )
        )
      )
    )
    where
      rLabel = rootLabel labels      
      nLabel = getLabelD "fromLabeledTreeNamedM<Maybe>: nothing label missing" [0] labels
      jLabel = getLabelD "fromLabeledTreeNamedM<Maybe>: just label missing" [1] labels 
      labels' = subtreeD "fromLabeledTreeNamedM<Maybe>: just subtree missing" [1,0] labels 
      
      l2 = getLabel [0] tree
      resM = (fromLabeledTreeNamedM labels' (LabeledTree (subtree [0,0] tree)))::(FromLabeledTree a => Maybe a) --output: Maybe a
    
{-
 TODO: make order-invariant and ignore additional subtrees
 if Left label and Right label exist then return Nothing (ambiguous)
 if more than one child of the root node has Left (or Right) label then return Nothing 
-}
instance (FromLabeledTree a, FromLabeledTree b) => FromLabeledTree (Either a b) where
  fromLabeledTreeNamedM labels (LabeledTree tree) = 
    (if' ((rootLabel tree) /= rLabel) Nothing
      (if' (not $ pathExists [0] tree) Nothing
        (if' (sideLabel /= leftLabel)        
          (if' (sideLabel /= rightLabel) Nothing
            (if' (not $ hasContent) Nothing
              --right
              (if' (isNothing rightResM) Nothing (Just (Right (just rightResM))))
            )
          )          
          (if' (not $ hasContent) Nothing
            --left
            (if' (isNothing leftResM) Nothing (Just (Left (just leftResM))))
          )
        )
      )
    )
    where
      hasContent = pathExists [0,0] tree
      sideLabel = getLabel [0] tree
      rLabel = rootLabel labels
      leftLabel = getLabelD "fromLabeledTreeNamedM<Either>: left label missing" [0] labels      
      rightLabel = getLabelD "fromLabeledTreeNamedM<Either>: right label missing" [1] labels
      lLabels' = subtreeD "fromLabeledTreeNamedM<Either>: left subtree missing" [0,0] labels
      rLabels' = subtreeD "fromLabeledTreeNamedM<Either>: right subtree missing" [1,0] labels
            
      resM labels' = fromLabeledTreeNamedM labels' (LabeledTree (subtree [0,0] tree))
      leftResM  = resM lLabels'      
      rightResM = resM rLabels'

{-
 TODO: make order-invariant and ignore additional subtrees
-}      
instance (FromLabeledTree a, Show a) => FromLabeledTree (Tree a) where
  fromLabeledTreeNamedM = undefined
  {-fromLabeledTreeNamedM labels (LabeledTree tree) = 
    (if' ((rootLabel tree) /= rLabel) Nothing
      
    )
    where
      rLabel = rootLabel labels 
      lLabel = getLabelD "fromLabeledTreeNamedM<Tree>: label label missing" [0] labels
      cLabel = getLabelD "fromLabeledTreeNamedM<Tree>: child list label missing" [1] labels
      labels' = getSubtreeD "fromLabeledTreeNamedM<Tree>: subtree of child list missing" [1,0] labels-}

--Tuple
instance (FromLabeledTree a1, FromLabeledTree a2) => FromLabeledTree (a1,a2) where
  fromLabeledTreeNamedM labels lt =
    (if' (rLabel /= l) Nothing
      (if' (((length c1) /= 1) || ((length c2) /= 1)) Nothing
        (if' ((isNothing r1M) || (isNothing r2M)) Nothing 
          (Just (just r1M,just r2M))
        )
      )    
    )
    where
      (LabeledTree (Tree l c)) = lt 
      
      rLabel = rootLabel labels
      a1Label = getLabel [0] labels
      a2Label = getLabel [1] labels
      
      labels1' = subtree [0] labels
      labels2' = subtree [1] labels

      c1 = filter (\t -> (rootLabel t) == a1Label) c
      c2 = filter (\t -> (rootLabel t) == a2Label) c

      r1M = fromLabeledTreeNamedM labels1' (LabeledTree (first c1))
      r2M = fromLabeledTreeNamedM labels2' (LabeledTree (first c2))
      
  
instance (FromLabeledTree a1, FromLabeledTree a2, FromLabeledTree a3) => FromLabeledTree (a1,a2,a3) where
  fromLabeledTreeNamedM labels lt =
    (if' (rLabel /= l) Nothing
      (if' (((length c1) /= 1) || ((length c2) /= 1) || ((length c3) /= 1)) Nothing
        (if' ((isNothing r1M) || (isNothing r2M) || (isNothing r3M)) Nothing 
          (Just (just r1M,just r2M,just r3M))
        )
      )    
    )
    where
      (LabeledTree (Tree l c)) = lt 
      
      rLabel = rootLabel labels
      a1Label = getLabel [0] labels
      a2Label = getLabel [1] labels
      a3Label = getLabel [2] labels
      
      labels1' = subtree [0] labels
      labels2' = subtree [1] labels
      labels3' = subtree [2] labels

      c1 = filter (\t -> (rootLabel t) == a1Label) c
      c2 = filter (\t -> (rootLabel t) == a2Label) c
      c3 = filter (\t -> (rootLabel t) == a3Label) c

      r1M = fromLabeledTreeNamedM labels1' (LabeledTree (first c1))
      r2M = fromLabeledTreeNamedM labels2' (LabeledTree (first c2))
      r3M = fromLabeledTreeNamedM labels3' (LabeledTree (first c3))
      
  
--Misc   
  
escapeChar = '\\'
escapeTable = [ ('\\',"\\\\"), ('\n',"\\n"), ('\t',"\\t") ] 
escapeTableTransposed = L.map f escapeTable
  where 
    f (x,y) = (y,x)

unescape  = apply unescapeP
unescapeD cl = (apply' ("SPLib.Tree.unescapeD @ "++cl)) unescapeP
unescapeP = PartialFunction "SPLib.Tree.unescapeP" f f_ood f_err
  where
    f = unescape_f
    f_ood = unescape_f_ood
    f_err = 
      [
        ["illegal escape sequence '","' in string ",""],
        ["escape character '\\' before end of string in string ",""]
      ]

--X START OF PROGRAM readLabeledTree    

--X END OF PROGRAM readLabeledTree    
        
--START OF PROGRAM escape

data Data_escape  = Data_escape 
  String --inp
  String --res
  Bool Bool
  
--SIGNATURES
escape :: String -> String

escape_o_copy :: Data_escape -> Data_escape
escape_o_end :: Data_escape -> Data_escape
escape_o_esc :: Data_escape -> Data_escape
escape_o_init :: Data_escape -> Data_escape
escape_o_nop :: Data_escape -> Data_escape

escape_p_eoi :: Data_escape -> Bool
escape_p_escape :: Data_escape -> Bool

--DEFINITIONS
escape inp = (traceProgramCall "escape" "escape" [("inp","String"),("res","String")]  res')
  where
    (Data_escape _ res' _ _) = (escape_o_init (Data_escape inp undefined True False))

escape_p_eoi (Data_escape inp res _ _) = isEmpty inp

escape_p_escape (Data_escape inp res _ _) = isJust (lookup (first inp) escapeTable)

escape_o_copy (Data_escape inp res inp_iD_ res_iD_) = (traceProgramOp "escape" "o_copy" data_ [True,True] flow_)
  where
    inp' = removeFirst inp
    res' = prepend (first inp) res
    inp_iDn_ = True
    res_iDn_ = True
    data_ = (Data_escape inp' res' inp_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "escape" "o_nop" (escape_o_nop data_))

escape_o_end (Data_escape inp res inp_iD_ res_iD_) = (traceProgramOp "escape" "o_end" data_ [False,True] flow_)
  where
    res' = reverse res
    inp' = inp
    res_iDn_ = True
    inp_iDn_ = inp_iD_
    data_ = (Data_escape inp' res' inp_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "escape" data_)

escape_o_esc (Data_escape inp res inp_iD_ res_iD_) = (traceProgramOp "escape" "o_esc" data_ [True,True] flow_)
  where
    inp' = removeFirst inp
    res' = (reverse (just (lookup (first inp) escapeTable))) ++ res
    inp_iDn_ = True
    res_iDn_ = True
    data_ = (Data_escape inp' res' inp_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "escape" "o_nop" (escape_o_nop data_))

escape_o_init (Data_escape inp res inp_iD_ res_iD_) = (traceProgramOp "escape" "o_init" data_ [False,True] flow_)
  where
    res' = ""
    inp' = inp
    res_iDn_ = True
    inp_iDn_ = inp_iD_
    data_ = (Data_escape inp' res' inp_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "escape" "o_nop" (escape_o_nop data_))

escape_o_nop (Data_escape inp res inp_iD_ res_iD_) = (traceProgramOp "escape" "o_nop" data_ [False,False] flow_)
  where
    inp' = inp
    res' = res
    inp_iDn_ = inp_iD_
    res_iDn_ = res_iD_
    data_ = (Data_escape inp' res' inp_iDn_ res_iDn_)
    p__ = (escape_p_eoi data_)
    p_1_ = (escape_p_escape data_)    
    flow_ = 
      (traceProgramPred "escape" "p_eoi" p__ (if' p__
        (traceProgramNextOp "escape" "o_end" (escape_o_end data_))
        (traceProgramPred "escape" "p_escape" p_1_ (if' p_1_
          (traceProgramNextOp "escape" "o_esc" (escape_o_esc data_))
          (traceProgramNextOp "escape" "o_copy" (escape_o_copy data_)) 
        )) 
      ))

--SHOW
instance Show Data_escape where  
  show (Data_escape inp res inp_iD_ res_iD_) = 
    (show [
      (if' inp_iD_ (prepend 'd' (show inp)) "u"),
      (if' res_iD_ (prepend 'd' (show res)) "u")
    ])

--END OF PROGRAM escape

--START OF PROGRAM unescape

data Data_unescape  = Data_unescape 
  [Error] --errs
  String --inp
  String --inp0
  String --res
  Bool Bool Bool Bool
  
--SIGNATURES
unescape_f :: String -> String
unescape_f_ood :: String -> [Error]

unescape_o_copy :: Data_unescape -> Data_unescape
unescape_o_end :: Data_unescape -> Data_unescape
unescape_o_err :: Data_unescape -> Data_unescape
unescape_o_esc1 :: Data_unescape -> Data_unescape
unescape_o_esc2 :: Data_unescape -> Data_unescape
unescape_o_init :: Data_unescape -> Data_unescape
unescape_o_nop :: Data_unescape -> Data_unescape

unescape_p_eoi :: Data_unescape -> Bool
unescape_p_err :: Data_unescape -> Bool
unescape_p_escape :: Data_unescape -> Bool
unescape_p_illegal :: Data_unescape -> Bool

--DEFINITIONS
unescape_f inp0 = (traceProgramCall "unescape_f" "unescape" [("errs","[Error]"),("inp","String"),("res","String")]  res')
  where
    (Data_unescape _ _ _ res' _ _ _ _) = (unescape_o_init (Data_unescape undefined undefined inp0 undefined False False True False))

unescape_f_ood inp0 = (traceProgramCall "unescape_f_ood" "unescape" [("errs","[Error]"),("inp","String"),("res","String")]  errs')
  where
    (Data_unescape errs' _ _ _ _ _ _ _) = (unescape_o_init (Data_unescape undefined undefined inp0 undefined False False True False))

unescape_p_eoi (Data_unescape errs inp inp0 res _ _ _ _) = isEmpty inp

unescape_p_err (Data_unescape errs inp inp0 res _ _ _ _) = not $ isEmpty errs

unescape_p_escape (Data_unescape errs inp inp0 res _ _ _ _) = (first inp) == escapeChar

unescape_p_illegal (Data_unescape errs inp inp0 res _ _ _ _) = (isNothing (lookup [escapeChar,(first inp)] escapeTableTransposed))

unescape_o_copy (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_copy" data_ [False,True,True] flow_)
  where
    inp' = removeFirst inp
    res' = prepend (first inp) res
    errs' = errs
    inp0' = inp0
    inp_iDn_ = True
    res_iDn_ = True
    errs_iDn_ = errs_iD_
    inp0_iDn_ = inp0_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "unescape" "o_nop" (unescape_o_nop data_))

unescape_o_end (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_end" data_ [False,False,True] flow_)
  where
    res' = reverse res
    errs' = errs
    inp' = inp
    inp0' = inp0
    res_iDn_ = True
    errs_iDn_ = errs_iD_
    inp_iDn_ = inp_iD_
    inp0_iDn_ = inp0_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "unescape" data_)

unescape_o_err (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_err" data_ [False,False,False] flow_)
  where
    errs' = errs
    inp' = inp
    inp0' = inp0
    res' = res
    errs_iDn_ = errs_iD_
    inp_iDn_ = inp_iD_
    inp0_iDn_ = inp0_iD_
    res_iDn_ = res_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramHalt "unescape" data_)

unescape_o_esc1 (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_esc1" data_ [True,True,False] flow_)
  where
    errs' = if' (isEmpty e1) e2 e1
        where
          e1 = conditionalError (isEmpty inp) (1,[(show inp0)])
          e2 =
            (conditionalError
              (isNothing (lookup [escapeChar,(first inp)] escapeTableTransposed))
              (0,[[escapeChar,(first inp)],(show inp0)])
            )
          inp = inp'
    inp' = removeFirst inp
    inp0' = inp0
    res' = res
    errs_iDn_ = True
    inp_iDn_ = True
    inp0_iDn_ = inp0_iD_
    res_iDn_ = res_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    p__ = (unescape_p_err data_)    
    flow_ = 
      (traceProgramPred "unescape" "p_err" p__ (if' p__
        (traceProgramNextOp "unescape" "o_err" (unescape_o_err data_))
        (traceProgramNextOp "unescape" "o_esc2" (unescape_o_esc2 data_)) 
      ))

unescape_o_esc2 (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_esc2" data_ [False,True,True] flow_)
  where
    inp' = removeFirst inp
    res' = prepend (just (lookup [escapeChar,(first inp)] escapeTableTransposed)) res
    errs' = errs
    inp0' = inp0
    inp_iDn_ = True
    res_iDn_ = True
    errs_iDn_ = errs_iD_
    inp0_iDn_ = inp0_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "unescape" "o_nop" (unescape_o_nop data_))

unescape_o_init (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_init" data_ [True,True,True] flow_)
  where
    errs' = []
    inp' = inp0
    res' = ""
    inp0' = inp0
    errs_iDn_ = True
    inp_iDn_ = True
    res_iDn_ = True
    inp0_iDn_ = inp0_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    
    flow_ = 
      (traceProgramNextOp "unescape" "o_nop" (unescape_o_nop data_))

unescape_o_nop (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = (traceProgramOp "unescape" "o_nop" data_ [False,False,False] flow_)
  where
    errs' = errs
    inp' = inp
    inp0' = inp0
    res' = res
    errs_iDn_ = errs_iD_
    inp_iDn_ = inp_iD_
    inp0_iDn_ = inp0_iD_
    res_iDn_ = res_iD_
    data_ = (Data_unescape errs' inp' inp0' res' errs_iDn_ inp_iDn_ inp0_iDn_ res_iDn_)
    p__ = (unescape_p_eoi data_)
    p_1_ = (unescape_p_escape data_)    
    flow_ = 
      (traceProgramPred "unescape" "p_eoi" p__ (if' p__
        (traceProgramNextOp "unescape" "o_end" (unescape_o_end data_))
        (traceProgramPred "unescape" "p_escape" p_1_ (if' p_1_
          (traceProgramNextOp "unescape" "o_esc1" (unescape_o_esc1 data_))
          (traceProgramNextOp "unescape" "o_copy" (unescape_o_copy data_)) 
        )) 
      ))

--SHOW
instance Show Data_unescape where  
  show (Data_unescape errs inp inp0 res errs_iD_ inp_iD_ inp0_iD_ res_iD_) = 
    (show [
      (if' errs_iD_ (prepend 'd' (show errs)) "u"),
      (if' inp_iD_ (prepend 'd' (show inp)) "u"),
      (if' res_iD_ (prepend 'd' (show res)) "u")
    ])

--END OF PROGRAM unescape
