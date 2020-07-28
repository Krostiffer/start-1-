module Breadth where 

import Prelude ()
import SPLib.Basic
import SPLib.List


data Graph  = Graph VertexCount EdgeList deriving (Show)
type VertexCount = Int
type EdgeList = [(Vertex,Vertex)]
type Vertex = Int

vertices  :: Graph -> [Vertex] --returns list [1..n] where n is the number of vertices
adjacent  :: Graph -> Vertex -> Vertex -> Bool 
neighbors :: Graph -> Vertex -> [Vertex]

vertices (Graph n _) = [1..n]          
adjacent (Graph _ el) u v = (contains (u,v) el) || (contains (v,u) el)
neighbors graph u = filter (adjacent graph u) (vertices graph)

testGraph = Graph 13 [(1,2),(1,3),(2,4),(2,5),(2,8),(3,5),(3,6),(3,7),(8,9),(9,10),(11,12),(12,13)]

  
--START OF PROGRAM bfs
--END OF PROGRAM bfs
