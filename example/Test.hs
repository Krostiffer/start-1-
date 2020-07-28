module Test where 

import Prelude (putStrLn,maxBound) 
import SPLib.Basic
import SPLib.List as L 
import SPLib.Map as M
import SPLib.Tree as T


--Misc 
f2   x y y' = (if' ((mod x 2) == 1) y y') --try :t f2 
f2'  x y y' = (if' ((mod (x::Int) 2) == 1) y y')
f2'' x y y' = (if' ((mod (x::Int) 2) == 1) (y::String) y')
f2''' x y y' = (if' ((mod (x::Int) 2) == 1) y y')::String

f2a       = f2 2       --always returns 2nd arg because 2 is even 
f2a' y y' = f2 2 y y'  --same as above  
f2b       = f2 1 "xx"  --always returns "xx"

doubleList list = map f list 
  where
    f x = x+x
    
doubleList' list = map (\x -> x+x) list --same as above using anonymous function
            
largerThan  y list = filter f list
  where
    f x = x > y
    
largerThan' y list = filter (>y) list --same as above 
smallerThan y list = filter (y>) list

{-
  if operators are passed as value then
  they must be enclosed in parenthesis
-}
product list  = reduce (*) list 
product' list = reduce f list
  where
    f x y = x*y 
    
factorial n = product [1..n] --expression [1..n] = [1,2,...,n]
    
f3 x = z
  where
    y = x + x
    z = y * y 

--equivalent to (assuming y,z are not defined on global level)
{-
f3 x = (z x)
y x = x + x
z x = (y x) * (y x)
-}

--List examples

--a string is a list of characters 
test1 = ['a'] == "a" 
test2 = ['a','b'] == "ab" 

list1 = [1,2,3,4,5]
list2 = "like"
list3 = [True,False,True,True]
list4 = [[(1,-2),(-3,4)],[(5,-6),(-7,8)]]  --2x2 matrix of pairs of integers

lt1 = first list1
lt2 = removeFirst list1
lt3 = last list1
lt4 = removeLast list1
lt5 = L.get 2 list1    --since Map also exports a function named get
lt6 = L.set 17 2 list1  --since Map also exports a function named set
lt7 = L.set 'h' 0 list2

lt8 = fst (L.get 1 (L.get 0 list4)) 

--Map examples
map1 :: (Map Int String)
map2 :: (Map Int String) 

map1_0 = M.empty --empty map
map1_1 = M.set "str3"  3  map1_0
map1_2 = M.set "str27" 27 map1_1
map1_3 = M.set "str14" 14 map1_2
map1_4 = M.set "str0" 0 map1_3
map1 = map1_4

--a Map can be constructed from a list of tuples 
map2 = fromList     -- map1 == map2
  [
    (0,"str0"),
    (3,"str3"),
    (14,"str14"),
    (27,"str27")
  ]

testGet = M.get 14 map1 -- "str14"
testHasKey1 = M.hasKey 2 map1 -- False
testHasKey2 = M.hasKey 0 map1 -- True
  

--Tree examples 
tree1 :: (Tree String) 
tree2 :: (Tree String)
tree3 :: (Tree Int)

tree1 = (Tree "" [
    (Tree "0" []),
    (Tree "1" [
      (Tree "1-0" []),      
      (Tree "1-1" [
        (Tree "1-1-0" [])
      ]),        
      (Tree "1-2" [])            
    ]),
    (Tree "2" [])
  ])
  
tree2 = (Tree "tree2" [Tree "child 1" [],Tree "child 2" []])

tree3 = (Tree 4 [Tree 12 [],Tree 12 [],(Tree (-2) [])])

testRootLabel = rootLabel tree1  

testGetLabel1 = getLabel [] tree1  
testGetLabel2 = getLabel [1,1,0] tree1  

testSetLabel1  = setLabel "test" [1,1,0] tree1  
testSetLabel2  = setLabel "test2" [] tree1  

testSubtree1 = subtree [1] tree1
testSubtree2 = subtree [1,1] tree1

testSize = T.size tree1

testAppendSubtree = appendSubtree tree2 [2] tree1
testAppendSubtree2 = appendSubtree tree2 [2] testAppendSubtree

--Data type and pattern matching examples

--pattern matching for integers 
fib 0 = 0
fib 1 = 1
fib x = (fib (x-1)) + (fib (x-2))

--pattern matching for tuples 
first2  (x,_) = x --same as fst
second2 (_,x) = x --same as snd
first3  (x,_,_) = x 
second3 (_,x,_) = x
third3  (_,_,x) = x

--pattern matching for trees
numberOfChildren (Tree rootLabel children) = length children

--matching on multiple arguments
match1 :: Bool -> [Int] -> Int
match1 True [x,y] = x
match1 True (x:y) = first y
match1 False [x,y] = y
match1 False (x:y) = x
match1 _ [] = 0

testTuple3 = ("string",True,14)
testFirst3  = first3 testTuple3
testSecond3 = second3 testTuple3
testThird3  = third3 testTuple3
{-
 SPLib.Basic.fst returns 1st component of a 2-tuple
 SPLib.Basic.snd returns 2nd component of a 2-tuple
-}

type Title = String
type Year = Int
type Duration = Int --duration in minutes
type AspectRatio = (Int,Int) --e.g. (16,9) or (4,3)

data MotionPicture = 
  Movie Title Year Genre Duration AspectRatio | 
  Series Title Year Genre [Episode] 
  deriving (Eq,Show,Read)
  
data Genre = Action | Crime | Drama | Thriller | Comedy 
  deriving (Eq,Show,Read,Enum)

data Episode = Episode Title Duration AspectRatio
  deriving (Eq,Show,Read)

myDatabase :: [MotionPicture]
myDatabase = 
  [
    Movie  "Victoria"     2015 Crime    138 (235,100),       
    Movie  "Oldboy"       2003 Action   120 (235,100),
    Movie  "A Sun"        2019 Drama    156 (16,9),
    Movie  "Memento"      2000 Thriller 113 (239,100),
    Movie  "12 Angry Men" 1957 Drama     96 (166,100),
    Movie  "Idiocracy"    2006 Comedy    84 (185,100),    
    Movie  "7 años"       2016 Thriller  77 (16,9),   
    Series "Oz"           1997 Crime [
      Episode "The Routine" 54 (133,100),
      Episode "Visits, Conjugal and Otherwise" 57 (133,100),
      Episode "God's Chillin'" 57 (133,100),
      Episode "Straight Life" 57 (133,100),
      Episode "To Your Health" 57 (133,100)
    ],
    Series "Cidade dos Homens" 2002 Drama [
      Episode "A Coroa do Imperador" 30 (4,3),
      Episode "O Cunhado do Cara" 30 (4,3),
      Episode "Correio" 30 (4,3),
      Episode "Uólace e João Victor" 30 (4,3)
    ]    
  ]

--myDatabase2 = read (show myDatabase2)  
myDatabase2 = (read "[Movie \"Victoria\" 2015 Crime 138 (235,100),Movie \"Oldboy\" 2003 Action 120 (235,100),Movie \"A Sun\" 2019 Drama 156 (16,9),Movie \"Memento\" 2000 Thriller 113 (239,100),Movie \"12 Angry Men\" 1957 Drama 96 (166,100),Movie \"Idiocracy\" 2006 Comedy 84 (185,100),Movie \"7 a\\241os\" 2016 Thriller 77 (16,9),Series \"Oz\" 1997 Crime [Episode \"The Routine\" 54 (133,100),Episode \"Visits, Conjugal and Otherwise\" 57 (133,100),Episode \"God's Chillin'\" 57 (133,100),Episode \"Straight Life\" 57 (133,100),Episode \"To Your Health\" 57 (133,100)],Series \"Cidade dos Homens\" 2002 Drama [Episode \"A Coroa do Imperador\" 30 (4,3),Episode \"O Cunhado do Cara\" 30 (4,3),Episode \"Correio\" 30 (4,3),Episode \"U\\243lace e Jo\\227o Victor\" 30 (4,3)]]")::[MotionPicture]

setTitle newTitle (Movie _ x1 x2 x3 x4) = (Movie newTitle x1 x2 x3 x4)
setTitle newTitle (Series _ x1 x2 x3) = (Series newTitle x1 x2 x3)

onlyMovies db = filter f db
  where
    f (Movie _ _ _ _ _) = True
    f _ = False

testSetTitle1 = setTitle "movie title" (L.get 0 myDatabase)
testSetTitle2 = setTitle "series title" (L.get 9 myDatabase)

getTitleOfSecondEp (Series _ _ _  (_:(((Episode x _ _):_)))) = x
testGetTitleOfSecondEp i = getTitleOfSecondEp (L.get i myDatabase)

durationBetween :: Duration -> Duration -> [MotionPicture] -> [MotionPicture]  
durationBetween x y db = filter f db
  where
    f (Movie _ _ _ dur _) = (x <= dur) && (dur <= y)
    f (Series _ _ _ eps) = (x <= dur) && (dur <= y)
      where
        dur = (reduce (+) (map getDuration eps)) --duration of a series is the sum of its episodes runtime
        getDuration (Episode _ dur _) = dur

        
--Tracing examples 
 
couple  :: String -> String -> [(Char,Char)] 
couple' :: String -> String -> [(Char,Char)] -- version of couple without pattern matching
coupleTraced :: String -> String -> [(Char,Char)] -- with tracing

couple (x:[]) (y:[]) = [(x,y)] --base case: both lists consist of a single element
couple (x:xRest) (y:yRest) = prepend (x,y) (couple xRest yRest) --recurse: both lists have at least two elements

couple' xLst yLst = 
  (if' (((length xLst)==1) && ((length yLst)==1))
      r1 --base case
      (if' (((length xLst)>1) && ((length yLst)>1))
        r2 --recurse 
        undefined --one of the two strings is empty or they have different lengths 
      )
  )
  where
    x = first xLst
    y = first yLst
    xRest = removeFirst xLst
    yRest = removeFirst yLst
    
    r1 = [(x,y)] 
    r2 = prepend (x,y) (couple xRest yRest)

couple'' xLst yLst = zip xLst yLst 
{-
  zip is a more general version of couple which works for lists of any type
  it also works for empty lists and when the lists have different lengths;
  check the type of couple'' with
    :t couple''
  since no type declaration for couple'' is defined, Haskell infers the
  most general type, which in this case is the same as that of zip
-}

coupleTraced (x:[]) (y:[]) = trace ("base case: "++(show res)) res
  where
    res = [(x,y)]
coupleTraced (x:xRest) (y:yRest) = trace ("recurse: "++(show res)) res 
  where
    res = prepend (x,y) (coupleTraced xRest yRest)


--initialize index with (length list) and res with []    
reverse2 list = reverse2H list (length list) []

{-
  while index > 0 append the (index-1)-th element 
  of list to res. return res when index = 0
-}
reverse2H list index res = 
  (if' (index==0)
    (trace "base case" res)
    (trace "recurse" (reverse2H list index' res'))
  )
  where
    index' = trace ("new index: "++(show x)) x
      where
        x = index - 1
    res' = trace ("new res:"++(show x)) x
      where
        x = append (L.get index' list) res
      
    
