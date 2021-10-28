package Graphs

import scala.collection.immutable.{HashMap, HashSet}

class Node[V]private(val value:V) extends AnyVal
class Graph3[V](val nodes:Map[Node[V],Set[Node[V]]]){
  def this() = this(new HashMap[Node[V],Set[Node[V]]])
  def this(graph3: Graph3[V]) = this(graph3.nodes)

  /**
   * Добавляет ноду, если такой нет
   * @return
   */
  def add(n:Node[V]):Graph3[V] = {
    if(nodes.contains(n))
      this
    else
      new Graph3[V](nodes+(n->new HashSet[Node[V]]()))
  }

  /**
   * Добавляет несколько ноду
   * @param ns
   * @return
   */
  def add(ns:Iterable[Node[V]]):Graph3[V] = {
  }
  /**
   * Добавляет ребро.
   * @return
   */
  def add(edge:(Node[V],Node[V])):Graph3[V] = {
    val n1e = nodes(edge._1) + edge._2
    val n2e = nodes(edge._2) + edge._1
    new Graph3[V](nodes + (edge._1->n1e) + (edge._2->n2e))
  }
/**
 * Добавляет несколько нод и реёбра между первой и остальными.
 */
  def add(node:Node[V],neighbours:Set[Node[V]]):Graph3[V] = {
    (neighbours + node).foreach(add)
    neighbours.foreach(n=>add((n,node)))
  }
}
































