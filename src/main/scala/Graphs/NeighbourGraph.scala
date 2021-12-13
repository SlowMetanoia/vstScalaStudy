package Graphs

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.immutable
import scala.language.postfixOps

class Node[V]private(val value:V) extends AnyVal{
  override def toString: String = value.toString
}
object Node{
  def apply[V](value:V):Node[V] = new Node(value)
}

/**
 * Честно говоря, теперь я понимаю, что в такой реализации гораздо логичнее ссылки на соседей поместить прямиком в ноды,
 * но переписывать сейчас не хочу.
 * @param nodes
 * @tparam V
 */
class NeighbourGraph[V](val nodes:Map[Node[V],immutable.Set[Node[V]]]) {
  def this() = this(new HashMap[Node[V], Set[Node[V]]])

  def this(graph3: NeighbourGraph[V]) = this(graph3.nodes)

  /**
   * Добавляет ноду, если такой нет
   * @return
   */
  def add(n: Node[V]): NeighbourGraph[V] = {
    if (nodes.contains(n))
      this
    else
      new NeighbourGraph[V](nodes + (n -> new HashSet[Node[V]]()))
  }

  /**
   * Добавляет несколько нод
   *
   * @param ns
   * @return
   */
  def add(ns: Iterable[Node[V]]): NeighbourGraph[V] = {
    //добавляем те ноды, которых ещё нет
    new NeighbourGraph[V](nodes ++ ns.view.filterNot(nodes.keySet.contains(_)).map(n => n -> new HashSet[Node[V]]()))
  }

  /**
   * Добавляет ребро.
   *
   * @return
   */
  def add(edge: (Node[V], Node[V])): NeighbourGraph[V] = {
    val n1e = nodes(edge._1) + edge._2
    val n2e = nodes(edge._2) + edge._1
    new NeighbourGraph[V](nodes + (edge._1 -> n1e) + (edge._2 -> n2e))
  }

  /**
   * Добавляет несколько рёбер
   *
   * @param edges
   * @return
   */
  def add(edges: Set[(Node[V], Node[V])]):NeighbourGraph[V] = {
    if (edges.nonEmpty) add(edges.head).add(edges.tail)
    else this
  }
  def addNodesByValues(values:V*):NeighbourGraph[V] = add(values.map(n=>Node(n)))
  def addEdgesByValues(values:(V,V)*):NeighbourGraph[V] = add(values.map(edge=>(Node(edge._1),Node(edge._2))).toSet)
  /**
   * Добавляет несколько нод и рёбра между первой и остальными.
   */
  def addNeighbours(node: Node[V], neighbours: Set[Node[V]]): NeighbourGraph[V] = add(neighbours + node).add(neighbours.map(n => (n, node)))

  /**
   * Делает то-же, что и add
   */
  def + (n: Node[V]): NeighbourGraph[V] = add(n)
  /**
   * Делает то-же, что и add
   */
  def + (edge: (Node[V], Node[V])): NeighbourGraph[V] = add(edge)
  /**
   * Делает то-же, что и add
   */
  def ++ (ns: Iterable[Node[V]]): NeighbourGraph[V] = add(ns)
  /**
   * Делает то-же, что и add
   */
  def ++ (edges: Set[(Node[V], Node[V])]): NeighbourGraph[V] = add(edges)

  /**
   * Исключает ноду из графа
   * @param n
   * @return
   */
  def exclude(n:Node[V]):NeighbourGraph[V] = new NeighbourGraph[V]((nodes - n).map(node=>
    if(node._2.contains(n))
      node._1->(node._2 - n)
    else
      node
  ))

  /**
   * То-же, что и exclude
   * @param n
   * @return граф без этой ноды
   */
  def - (n:Node[V]):NeighbourGraph[V] = exclude(n)

  /**
   * Делит граф на компоненты связности
   * @return
   */
  def connectedGraphParts:Seq[NeighbourGraph[V]] = {
    //можно и с коллекциями, но так производительнее
    val nodeSet = nodes.keySet.toArray
    val used = new Array[Boolean](nodeSet.length)
    val result = for (i<-nodeSet.indices if !used(i)) yield {
      val part = getConnectedToNode(nodeSet(i))
      part.foreach(n=>used(nodeSet.indexOf(n))=true)
      part
    }
    result.map(ns=>new NeighbourGraph[V](ns.map(node=>node->nodes(node)).toMap))
  }

  def findBottlenecks:Set[Node[V]] = {
    for {
      node <- nodes.keySet
      if connectedGraphParts.size < exclude(node).connectedGraphParts.size
    } yield node
  }

  /**
   * @param nodeSet набор связных нод
   * @return полный набор нод, образующих связный подграф
   */
  @tailrec
  private[this] def getConnectedPart(nodeSet:Set[Node[V]]):Set[Node[V]] ={
    val newNodes = for(node<-nodeSet;neighbour<-nodes(node) if !nodeSet.contains(neighbour)) yield neighbour
    if(newNodes.isEmpty)
      nodeSet
    else
      getConnectedPart(nodeSet ++ newNodes)
  }
  /**
   * @return полный набор нод, связанных с данной, образующих связный подграф
   */
  private[this] def getConnectedToNode:Node[V]=>Set[Node[V]] = n=> getConnectedPart(Set(n))

  /**
   * Реализация двунаправленного поиска
   * @param node1
   * @param node2
   * @return графовое растояние между node1 и node2
   */
  def ErdosNumber(node1:Node[V],node2:Node[V]): Int = {
    //обход в ширину
    def expand(nodeSet:Set[Node[V]],surface:Set[Node[V]]):(Set[Node[V]],Set[Node[V]]) = {
      val newNodes = for (node<-surface;n<-nodes(node) if !nodeSet.contains(n)) yield n
      (nodeSet++newNodes,newNodes)
    }
    var counter = 0
    var x = (Set(node1),Set(node1))
    var y = (Set(node2),Set(node2))
    while (!((x._2.isEmpty) || (y._2.isEmpty))) {
      if ((x._2 & y._2).nonEmpty) return counter
      counter+=1
      x = expand(x._1,x._2)
      if ((x._2 & y._2).nonEmpty) return counter
      y = expand(y._1,y._2)
      counter+=1
    }
    -1
  }
  override def toString = s"\n${this.getClass}:\n" + (for((n,ns)<-nodes) yield s"$n->${ns.mkString("(",",",")")}").mkString("\n")
}