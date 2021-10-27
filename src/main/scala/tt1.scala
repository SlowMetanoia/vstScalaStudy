

import scala.collection.mutable.ArrayBuffer

object tt1 {
  def main(args: Array[String]): Unit = {
    Graph.main(new Array[String](0))
  }

  class Graph(val description:String) {
    val nodes = new ArrayBuffer[Node]
    val edges = new ArrayBuffer[Edge]
    private val findBottleNeck: ArrayBuffer[Node] => ArrayBuffer[Edge] => ArrayBuffer[Node] = nodes => edges => {
      val weights = for (node <- nodes) yield (countNeighbors(node), node)
      var maxW = -1
      for ((weight, node) <- weights if weight > maxW) maxW = weight
      for ((weight, node) <- weights if weight == maxW) yield node
    }

    def NewNodes(strs: String*): Unit = nodes ++= (for (s <- strs if !findNode(s)) yield new Node(s))

    def NewEdges(edges: (String, String, String)*): Unit = {
      this.edges ++= (for ((n1, n2, desc) <- edges if findNode(n1) && findNode(n2) && (n1 != n2)) yield
        new Edge(GetNodeByDecryption(n1), GetNodeByDecryption(n2), desc))
    }

    def findNode(description: String): Boolean = {
      nodes.foreach(n => if (n.description == description) return true)
      false
    }

    def GetNodeByDecryption(description: String): Node = {
      for (n <- nodes if (n.description == description)) return n
      null
    }

    def countNeighbors(node: Node): Int = edges.filter(e => e.contains(node)).size

    def printMe: Unit = {
      println(this.toString)
    }

    override def toString = s"Graph($description):\nNodes:\n${nodes.mkString("\n")}\nEdges:\n ${edges.mkString("\n")}"

    class Node(val description: String) {
      override def toString = s"Node($description)"
    }

    class Edge(val source: Node, val receiver: Node, val description: String) {
      def contains(node: Node) = (node == source) || (node == receiver)

      override def toString = s"Edge($source, $receiver, $description)"
    }
  }

  object Graph {
    def main(args: Array[String]): Unit = {
      val graph = new Graph("test graph")
      graph.NewNodes("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
      graph.NewEdges(("0", "1", "01"), ("1", "2", "12"), ("1", "3", "13"), ("3", "4", "34"))
      graph.NewEdges(("0", "1", "01"), ("1", "2", "12"), ("1", "3", "13"), ("3", "4", "34"))
      graph.printMe
      println(graph.findBottleNeck(graph.nodes)(graph.edges))
    }
  }
}