package Graphs

object main extends App{
  var graph3 = new NeighbourGraph[Int]()
  graph3 = graph3.addNodesByValues(1,2,3,4,5,6,7)
  graph3 = graph3.addNodesByValues(1,2,3,4,5,6,7)
  graph3 = graph3.addEdgesByValues((1,2),(1,3),(2,3),(4,6),(6,5))
  println("This graph:" + graph3)
  println("Connected parts:" + graph3.connectedGraphParts.mkString("\n"))
  println("Bottlenecks:" + graph3.findBottlenecks)
}
