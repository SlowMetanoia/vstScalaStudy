var graph3 = new Graphs.NeighbourGraph[Int]()
graph3 = graph3.addNodesByValues(1,2,3,4,5,6,7)
graph3 = graph3.addNodesByValues(1,2,3,4,5,6,7)
graph3 = graph3.addEdgesByValues((1,2),(1,3),(2,3),(4,6),(6,5))
graph3.connectedGraphParts
