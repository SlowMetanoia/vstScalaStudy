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
   * Добавляет несколько нод
   * @param ns
   * @return
   */
  def add(ns:Iterable[Node[V]]):Graph3[V] = {
    //добавляем те ноды, которых ещё нет
    new Graph3[V](nodes ++ ns.view.filterNot(nodes.keySet.contains(_)).map(n=>n->new HashSet[Node[V]]()).force)
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
   * Добавляет несколько рёбер
   * @param edges
   * @return
   */
  def add(edges:Set[(Node[V],Node[V])]):Graph3[V] = {
    var by_1 = edges.groupBy(_._1)
    val by_2 = edges.groupBy(_._2)
    by_2.keySet.foreach(n=>{

      if (by_1.contains())

    }
    )
    for((k,_)<-by_1 if by_2.contains(k)) by_1 = by_1 + (k->(by_1(k) + by_2(k)))
    ???
  }
/**
 * Добавляет несколько нод и реёбра между первой и остальными.
 */
  def add(node:Node[V],neighbours:Set[Node[V]]):Graph3[V] = {
    ???
  }
}
































