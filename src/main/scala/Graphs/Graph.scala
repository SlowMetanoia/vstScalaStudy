package Graphs

import scala.collection.{immutable, mutable}
import scala.collection.immutable.{HashMap, HashSet}

/**
 * какая-то реализмция ненаправленного графа
 */
class Node[V]private(val value:V) extends AnyVal
object Node{
  def apply[V](value:V) = new Node[V](value)
}
class Graph[V,E] {
  /**
   * @param nodes
   * @param neighbours
   */
  def this(nodes:immutable.HashSet[Node[V]],
           neighbours:immutable.HashMap[Node[V],immutable.HashSet[Node[V]]]){
    this()
    this.nodes = nodes
    this.neighbours = neighbours
  }
  var nodes       = new immutable.HashSet[Node[V]]()
  var neighbours  = new immutable.HashMap[Node[V],immutable.HashSet[Node[V]]]()

  /**
   * Затирает старую ноду вместе со всеми её связями или создаёт новую
   * @param node
   * @param newNeighbours
   */
  def add(node:V,newNeighbours:V*) = {
    nodes = nodes + Node(node) ++ newNeighbours.map(Node(_))
    neighbours = neighbours + (Node(node)->(HashSet() ++ newNeighbours.map(Node(_))))
  }

  /**
   * Делит граф на компоненты связности
   * @return
   */
  def SeparateByConnectedParts = {
    var calculatedNodes = new immutable.HashMap[Node[V],Int]()
    def separatePartWith:Node[V]=>Int=>Unit = n=>i=> {
      calculatedNodes = calculatedNodes + (n->i)
      (neighbours(n) &~ calculatedNodes.keySet).map(separatePartWith(_)(i))
    }
    var counter = 0
    //я надеюсь, он проверяет на каждой итерации, а не фильтрует в начале.
    //вопрос, можно ли тут писать for(node<-nodes &~ calculatedNodes.keySet) - остаётся открытым.
    for(node<-nodes if !calculatedNodes.keySet.contains(node)){
      separatePartWith(node)(counter)
      counter+=1
    }
  }
}
