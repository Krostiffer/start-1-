module Types where

import Prelude ((!!))
import SPLib.Basic (Show(show), Read, Eq, Char, String, Bool(True,False), Int, if',not,undefined, error, (==), (/=), (+), (-), (<), (++), snd, fst, max)
import SPLib.List as L (isEmpty, prepend, append, contains, set, get, removeLast, last, removeDuplicates, length, map, concat, drop, take)
import SPLib.Map  as M (Map, toList, fromList, get, hasKey)


-- Graph used in AB2_3

type Vertex = Int
type OutNeighbors = [Vertex]
data Graph = Graph [(Vertex,OutNeighbors)] deriving (Eq, Show, Read)

isEmptyGraph :: Graph -> Bool --true iff graph has no vertices
vertices :: Graph -> [Vertex] --all vertices of the graph

isEmptyGraph (Graph []) = True
isEmptyGraph (Graph _) = False
vertices (Graph al) = map fst al


-- BooleanFunction used in AB2_5

type Arity = Int
data BooleanFunction = BooleanFunction ([Bool] -> Bool) [Bool] Arity

eval         :: [Bool] -> BooleanFunction -> Bool
partialApply :: Bool -> BooleanFunction -> BooleanFunction 
arity        :: BooleanFunction -> Int

eval x (BooleanFunction f y k) = 
  (if' (l /= k) (error ("eval: invalid number of arguments"))
    f (y++x)
  )
  where 
    l = length x   
    
partialApply x (BooleanFunction f y k) = 
  (if' (k < 1) (error ("partialApply: function has no more arguments to fix"))
    (BooleanFunction f (append x y) (k-1))
  )

arity (BooleanFunction _ _ k) = k   


-- CSMInstance used in AB3_4

data CSMInstance = CSMInstance [String] [String] deriving (Eq)

createCSM :: [String] -> [String] -> CSMInstance
coa  :: CSMInstance -> Int --number of strings in a
cob  :: CSMInstance -> Int --number of strings in b
lena :: CSMInstance -> Int -> Int --length of (i+1)-th string in a
lenb :: CSMInstance -> Int -> Int --length of (i+1)-th string in b
ssc  :: CSMInstance -> Int -> Int -> Int -> Int -> Int -> Bool --true iff 

createCSM a b = (CSMInstance a b)
coa (CSMInstance a b) = length a
cob (CSMInstance a b) = length b
lena (CSMInstance a b) i = length (a!!i)
lenb (CSMInstance a b) i = length (b!!i)
ssc (CSMInstance a b) ca cb oa ob l = (take l (drop oa (a!!ca))) == (take l (drop ob (b!!cb)))
                            
instance Show CSMInstance where
    show (CSMInstance a b) = (show (a,b)) 
    
    
-- DFA & NFA used in AB4

type State       = String
type StartState  = State
type FinalStates = [State]
type DeltaFun    = Map (State,Char) State
type DeltaRel    = Map (State,Char) [State]

undefinedState = "UNDEFINED!"

data DFA = DFA StartState FinalStates DeltaFun deriving (Eq,Show,Read)
data NFA = NFA StartState FinalStates DeltaRel deriving (Eq,Show,Read)

nextState  :: DFA -> State -> Char -> State   --if no transition then undefinedState
nextStates :: NFA -> State -> Char -> [State] --if no transition then []

isUndefinedTransition :: DFA -> State -> Char -> Bool

class FSA0 a where
  startState   :: a -> State  
  isFinalState :: a -> State -> Bool    
  states       :: a -> [State] 

instance FSA0 DFA where
  startState (DFA x _ _) = x  
  isFinalState (DFA _ x _) y = contains y x
  states (DFA ss _ d) = removeDuplicates (prepend ss (map snd (toList d)))
  
instance FSA0 NFA where
  startState (NFA x _ _) = x
  isFinalState (NFA _ x _) y = contains y x
  states (NFA ss _ d) = removeDuplicates (prepend ss (concat (map snd (toList d))))
  
nextState  (DFA _ _ d) z x = if' (hasKey (z,x) d) (M.get (z,x) d) undefinedState
nextStates (NFA _ _ d) z x = if' (hasKey (z,x) d) (M.get (z,x) d) []

isUndefinedTransition (DFA _ _ d) z x = not (hasKey (z,x) d)
  

-- CounterMachine used in AB6
data CM = CM [Int] deriving (Eq, Show, Read)

inc    :: CM -> Int -> CM
dec    :: CM -> Int -> CM 
isZero :: CM -> Int -> Bool 

inc (CM x) i = CM (L.set ((L.get (i-1) x)+1) (i-1) x)
dec (CM x) i = CM (L.set (max 0 ((L.get (i-1) x)-1)) (i-1) x)
isZero (CM x) i = (L.get (i-1) x) == 0


-- StackMachine used in AB6
data SM = SM [String] deriving (Eq, Show, Read)

push    :: SM -> Int -> Char -> SM
pop     :: SM -> Int -> SM
isEmpty :: SM -> Int -> Bool
isChar  :: SM -> Int -> Char -> Bool

push (SM x) i y = (SM (L.set (append y (L.get (i-1) x)) (i-1) x))
pop (SM x) i = (SM (L.set (rl (L.get (i-1) x)) (i-1) x))
  where
    rl "" = ""
    rl s = removeLast s
isEmpty (SM x) i = L.isEmpty (L.get (i-1) x)
isChar (SM x) i y = 
  (if' (Types.isEmpty (SM x) i) 
    False
    ((L.last (L.get (i-1) x)) == y)
  )


-- TuringMachine used in AB7
type WorkTape = (String,Int) --1st = tape content, 2nd = current cell
type OutputTape = String --content of output tape 
data TM = TM WorkTape OutputTape deriving (Eq, Show, Read)

left   :: TM -> TM --moves head on work tape one cell to the left
right  :: TM -> TM --moves head on work tape one cell to the right
write  :: TM -> Char -> TM --writes char to current cell
read   :: TM -> Char -> Bool --returns true iff current cell content on work tape equals char 
output :: TM -> Char -> TM --write char to output tape

init :: String -> TM --initializes TM with string on work tape (current cell = first char of string) and empty output tape
getOutput :: TM -> String --returns string on output tape

blank = '_'

left (TM (w,i) y) = if' (i==0) (TM (prepend blank w,0) y) (TM (w,i-1) y)
right (TM (w,i) y) = if' (i==(n-1)) (TM (append blank w,n) y) (TM (w,i+1) y)
  where
    n = length w
write (TM (w,i) y) z = TM ((set z i w),i) y
read (TM (w,i) _) z = z == (L.get i w)
output (TM x y) z = TM x (append z y)
init x = TM (x,0) ""
getOutput (TM _ y) = y
