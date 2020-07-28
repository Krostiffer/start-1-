#NAME 
bfs

#FUNCS
connected graph u v = res
component graph u = visited

#VARS
graph :: Graph
u     :: Vertex
v     :: Vertex

visited :: [Vertex]
toVisit :: [Vertex]

res :: Bool 

#PREDS
p_empty = isEmpty toVisit
p_unused = --this predicate is never used during execution and only serves as example 
  (((isEmpty visited) 
  || (not (isEmpty toVisit))) 
  && (u > 0))
     

#OPS
o_init:
  visited' = []
  toVisit' = [u]

o_visit:    
  cur = first toVisit     
  visited' = (append cur visited)  
  
  toVisit' = 
    (removeFirst toVisit) --removes cur from toVisit
    ++ (filter notVisitedYet (neighbors graph cur)) --add all neighbors of cur which have not been visited yet
                                                    --and which are not in toVisit 
      
  --helper variable which contains a function   
  notVisitedYet u = (not (contains u visited)) && (not (contains u toVisit)) 
    
o_nop: --this operation is never used during execution and only serves as example

o_pat: --this operation is never used during execution and only serves as example
  (x,_) = (1,2) --pattern matching is possible with helper variables
  u' = x        --then assign helper variable to program variable   
  --(u',_) = (1,2) --this would not work as expected  
    
o_res:
  res' = contains v visited --iff u is connected to v

#FLOW
o_init = o_visit
o_visit = (p_empty o_res o_visit)
o_res = HALT --indicates that the program should stop

o_nop = --not used during program execution and only serves as example
  (p_unused
    (p_empty --if p_empty is true go to o_init, else o_visit
      o_init  
      o_visit
    ) 
    (p_empty 
      o_res 
      HALT 
    )
  )
  --program halts after o_nop iff p_unused and p_empty are both false 

o_pat = o_pat --not used during program execution  
  