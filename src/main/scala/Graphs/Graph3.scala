package Graphs

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.{immutable, mutable}

class Node[V]private(val value:V) extends AnyVal
class Graph3[V](val nodes:Map[Node[V],immutable.Set[Node[V]]]) {
  def this() = this(new HashMap[Node[V], Set[Node[V]]])

  def this(graph3: Graph3[V]) = this(graph3.nodes)

  /**
   * Добавляет ноду, если такой нет
   *
   * @return
   */
  def add(n: Node[V]): Graph3[V] = {
    if (nodes.contains(n))
      this
    else
      new Graph3[V](nodes + (n -> new HashSet[Node[V]]()))
  }

  /**
   * Добавляет несколько нод
   *
   * @param ns
   * @return
   */
  def add(ns: Iterable[Node[V]]): Graph3[V] = {
    //добавляем те ноды, которых ещё нет
    new Graph3[V](nodes ++ ns.view.filterNot(nodes.keySet.contains(_)).map(n => n -> new HashSet[Node[V]]()).force)
  }

  /**
   * Добавляет ребро.
   *
   * @return
   */
  def add(edge: (Node[V], Node[V])): Graph3[V] = {
    val n1e = nodes(edge._1) + edge._2
    val n2e = nodes(edge._2) + edge._1
    new Graph3[V](nodes + (edge._1 -> n1e) + (edge._2 -> n2e))
  }

  /**
   * Добавляет несколько рёбер
   *
   * @param edges
   * @return
   */
  def add(edges: Set[(Node[V], Node[V])]): Graph3[V] = {
    case Nil => this
    //TODO: Refactor this:
    case edges: Set[(Node[V], Node[V])] => add(edges.head).add(edges.tail)
  }

  /**
   * Добавляет несколько нод и рёбра между первой и остальными.
   */
  def addNeighbours(node: Node[V], neighbours: Set[Node[V]]): Graph3[V] = add(neighbours + node).add(neighbours.map(n => (n, node)))

  /**
   * Делает то-же, что и add
   */
  def + (n: Node[V]): Graph3[V] = add(n)
  /**
   * Делает то-же, что и add
   */
  def + (edge: (Node[V], Node[V])): Graph3[V] = add(edge)
  /**
   * Делает то-же, что и add
   */
  def ++ (ns: Iterable[Node[V]]): Graph3[V] = add(ns)
  /**
   * Делает то-же, что и add
   */
  def ++ (edges: Set[(Node[V], Node[V])]): Graph3[V] = add(edges)

  /**
   * Исключает ноду из графа
   * @param n
   * @return
   */
  def exclude(n:Node[V]):Graph3[V] = new Graph3[V](nodes - n)

  /**
   * То-же, что и exclude
   * @param n
   * @return
   */
  def - (n:Node[V]):Graph3[V] = exclude(n)

  /**
   * Делит граф на компоненты связности
   * @return
   */
  def getConnected:Set[Graph3[V]] = {
    val doneNodes
    val separation = new mutable.HashMap[Int,mutable.HashSet[Node[V]]]
    var counter = 0
    def addNear(near:Set[Node[V]],i:Int):Graph3[V] = {
      val newNodes:immutable.Set[Node[V]] = (separation(i) &~ near).toSet
      if (newNodes.nonEmpty) {
        separation(i) ++= newNodes
        val newNear:immutable.Set[Node[V]] = for{
          old<-newNodes
          node<-nodes(old)
          if !separation(i).contains(node)
        } yield node
        counter+=1
        addNear(newNear,i)
      }
    }
  }
}

