package Ex

import scala.collection.mutable.ArrayBuffer

object tt2 extends App{
  class Graph(description: String){
    val nodes = new ArrayBuffer[Node]
    class Node(val description:String){
      val neighbours = new ArrayBuffer[Node]
      def addEdgesToNeighbours(nodes:Node*):Unit = {
        this.neighbours ++= nodes
        for(node<-nodes) node.neighbours += this
      }
    }
    def addNode(name:String,names:String*):Unit={
      def getNode(name:String): Node = {
        for(n<-nodes) if (n.description==name) return n
        return new Node(name)
      }
      val node = getNode(name)
      nodes+=node
      node.neighbours ++= for(n<-names) yield getNode(n)
    }
    //однонаправленный поиск
    def findGraphDistance(node1:Node,node2:Node):Int = {
      var surface = ArrayBuffer(node1)  //поверхность
      var frontier = node1.neighbours   //слой точек над поверхностью
      var counter = 0
      do{
        if (surface.contains(node2)) return counter //если нужная точка захвачена, возвращаем счётчик
        counter+=1
        val oldSurface = surface                    //старая поверхность
        surface = frontier                          //новая поверхность
        frontier = new ArrayBuffer[Node]            //зануляем фронтир
        def isNew(node:Node): Boolean = {
          (!frontier.contains(node))&&(!surface.contains(node))&&(!oldSurface.contains(node))
        }       //проверка точки на принадлежность к фронтиру
        //генерация нового фронтира
        frontier = for(parent<-surface; node<-parent.neighbours if isNew(node)) yield node
      } while(frontier.size != 0)
      new Nothing
    }
    //двунаправленный поиск
    //как функция выше, но в среднем эффективнее
    def findGraphDistance2(node1:Node,node2:Node):Int ={
      var surface1 = ArrayBuffer(node1)  //поверхности
      var surface2 = ArrayBuffer(node2)
      var frontier1 = node1.neighbours   //слои точек над поверхностью
      var frontier2 = node2.neighbours
      var counter = 0                   //счётчик
      def areSurfacesTouching:Boolean = {
        for(n<-surface1 if surface2.contains(n)) return true
        false
      }
      do {
        if(areSurfacesTouching) return counter
        counter+=1
        if(counter%2==0){
          val oldSurface1 = surface1
          surface1 = frontier1
          frontier1 = new ArrayBuffer[Node]()
          def isNew1(node:Node) = {
            (!frontier1.contains(node))&&(!surface1.contains(node))&&(!oldSurface1.contains(node))
          }
          frontier1 = for(parent<-surface1; node<-parent.neighbours if isNew1(node)) yield node
        }else{
          val oldSurface2 = surface2
          surface2 = frontier2
          frontier2 = new ArrayBuffer[Node]()
          def isNew2(node:Node) = {
            (!frontier2.contains(node))&&(!surface2.contains(node))&&(!oldSurface2.contains(node))
          }
          frontier2 = for(parent<-surface2; node<-parent.neighbours if isNew2(node))yield node
        }
     }while((frontier1.size>0)&&(frontier2.size>0))
      new Nothing
    }
  }

}
