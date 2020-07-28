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

acyclic = undefined
reduction = undefined
inNeighbors = undefined
outNeighbors = undefined
