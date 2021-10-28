package Graphs

import scala.collection.immutable.{HashMap, HashSet}

/**
 * нода какого-то типа
 * @param value
 * @tparam V
 */
class Node[V]private(val value:V) extends AnyVal
object Node {
  def apply[V](value:V) = new Node(value)
}
/**
 * ненаправленный граф с нодами какогог-то типа.
 * Прототип.
 */
class Graph2[V] {
  def this(nodes:HashMap[Node[V],HashSet[Node[V]]]) = {
    this()
    this.nodes = nodes
  }
  def this(graph: Graph2[V]) = {
    this(graph.nodes)
  }
  var nodes = new HashMap[Node[V],HashSet[Node[V]]]()

  /**
   * Добавляет ноду, если такой нет
   * @param node
   */
  def add(node:Node[V]):Unit = nodes.getOrElse(node,{nodes = nodes + node})

  /**
   * Добавляет ребро
   * @param edge
   */
  def add(edge:(Node[V],Node[V])) = {
    nodes = nodes + (edge._1->(nodes(edge._1) + edge._2))
    nodes = nodes + (edge._2->(nodes(edge._2) + edge._1))
  }

  /**
   * Добавляет несколько нод и реёбра между первой и остальными.
   * @param node
   * @param neighbours
   */
  def add(node:Node[V],neighbours:Set[Node[V]]) = {
    //добавляем все ноды, которых нет
    (neighbours + node).foreach(add(_))
    neighbours.foreach(n=>add((n,node)))
  }
  /**
   * Убрать ноду и все её рёбра
   * @param node
   * @return
   */
  def exclude(node:Node[V]) = {
    nodes(node).foreach(n=>exclude((node,n)))
    nodes = nodes - node
  }
  /**
   * Убрать ребро
   * @param edge
   */
  def exclude(edge:(Node[V],Node[V])) = {
    nodes = nodes + (edge._1->(nodes(edge._1) - edge._2))
    nodes = nodes + (edge._2->(nodes(edge._2) - edge._1))
  }
  def SeparateByConnectedParts:Unit = {
    var doneNodes = new HashSet[Node[V]]
    def getSeparatedPart(node:Node[V]):Graph2[V] = {
      doneNodes = doneNodes + node

    }
  }
}
