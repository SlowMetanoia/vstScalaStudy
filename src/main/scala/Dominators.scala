import Graphs.NeighbourGraph

object Dominators extends App{
  var graph = new NeighbourGraph[Int]
  graph = graph.addNodesByValues(1,2,3,4,5,6)
  graph = graph.addEdgesByValues((1,2),(2,3),(1,3),(3,4),(4,6),(3,5),(5,6))
  println(s"In graph$graph \nbottlenecks are nodes:${graph.findBottlenecks.mkString("(",",",")")}")
}