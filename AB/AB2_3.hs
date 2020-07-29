module AB2_3 where

import Prelude ()
import Types (Graph(Graph),Vertex,isEmptyGraph,vertices)
import SPLib.Basic (Eq, Int, Bool (True,False), trace, undefined, if', not, fst, snd, (==), (/=), (&&), (||))
import SPLib.List (isEmpty, map, foldl, filter, contains, find)


inNeighbors  :: Vertex -> Graph -> [Vertex]
outNeighbors :: Vertex -> Graph -> [Vertex]
reduction    :: Graph -> Graph
acyclic      :: Graph -> Bool

-- *** DO NOT MODIFY ABOVE CODE ***



inNeighbors v (Graph g) = map fst (filter f g) 
    where 
        f x = contains v (snd x) 

outNeighbors v (Graph g) = h (map snd (filter f g))
    where 
        f x = v == (fst x) 
        h x = if' (isEmpty x) [] (unpack x) --checks for empty outneighborlist
        unpack [x] = x --map(filter()) returns the single list of outneighbors in another list, it has to be unpacked

reduction (Graph g) = Graph (map (f') (filter f g))
  where
      f s = not (isEmpty (inNeighbors (fst s) (Graph g)) || isEmpty (outNeighbors (fst s) (Graph g)))
      f' el = ((fst el), (filter h (snd el)))
        where 
            h x = contains x (vertices (Graph(filter f g)))

acyclic (Graph g) = if' (isEmptyGraph (Graph g)) True (if' ((Graph g) == (reduction (Graph g))) False (acyclic (reduction (Graph g))))

