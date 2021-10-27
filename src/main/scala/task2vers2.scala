import scala.collection.mutable

object task2vers2 {
  def main(args: Array[String]): Unit ={

  }
  class Graph{
    class Node(val description: String){
      //override def equals(that: Any): Boolean = this.description==that.description
      val neighbours = new mutable.HashSet[Node]
    }
  }
}
