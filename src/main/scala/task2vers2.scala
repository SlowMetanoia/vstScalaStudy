import scala.collection.mutable

object task2vers2 {
  def main(args: Array[String]): Unit ={

  }
  class Graph{
    import scala.collection.mutable
    val nodes = new mutable.HashSet[Node]
    class Node(val description: String){
      override def equals(that: Any): Boolean = that match{
        case n:Node =>n.description == description
        case _=>false
      }
      override def hashCode():Int = description.##
      val neighbours = new mutable.HashSet[Node]
    }
    def LinkNodes(n1:Node,n2:Node) = {
      n1.neighbours.addOne(n2)
      n2.neighbours.addOne(n1)
    }
    def addNode(n:Node,newNodes:Node*):Unit = {
      nodes += n
      nodes ++= newNodes
      newNodes.foreach(node=>LinkNodes(n,node))
    }
    //я всё ещё не знаю, как расщифровывать 3 последние символа и почему так. Nevertheless...
    def addNode(n:String,newNodes:String*):Unit = addNode(new Node(n), (for(node<-newNodes) yield new Node(node)):_*)
  }
}
