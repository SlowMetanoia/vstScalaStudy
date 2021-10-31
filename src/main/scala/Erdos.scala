import Graphs.{NeighbourGraph, Node}

object Erdos extends App{
  var graph = new NeighbourGraph[String]()
  def inputAdapter(strs:String*): Unit ={
    val newNodes = strs.map(n=>Node(n))
    graph = graph ++ newNodes
    val newEdges = (for(n1<-newNodes;n2<-newNodes if n1 != n2) yield (n1,n2)).toSet
    graph = graph ++ newEdges
  }
  inputAdapter("Smith","Martin","Erdos")
  inputAdapter("Erdos","Reisig")
  inputAdapter("Smith","Chan")
  inputAdapter("Jablonski","Hsueh")
  inputAdapter("Chan","Li")
  println(s"In Graph:\n$graph"+"\nErdos number of Erdos and Hsueh = "+graph.ErdosNumber(Node("Erdos"),Node("Hsueh")))
}
