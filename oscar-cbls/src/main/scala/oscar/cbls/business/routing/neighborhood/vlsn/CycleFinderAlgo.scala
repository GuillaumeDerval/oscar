package oscar.cbls.business.routing.neighborhood.vlsn



object MatrixTools{
  def printBoolMatrix(m:Array[Array[Boolean]]): Unit ={
    for(l <- m){
      println(l.mkString("\t"))
    }
  }
}

object CycleFinderAlgoTest extends App{
  val graph = VLSNGraphTest.buildGraph()
  println(graph)

  println("starting DFS")
  val cycle = new CycleFinderAlgoDFS(graph,false).findCycle()
  println("done DFS")
  println(cycle)


  println("starting Moutuy")
  val cycle2 = new CycleFinderAlgoMouthy(graph:VLSNGraph).findCycle()
  println("done Moutuy")
  println("cycle found: " + cycle2)

//  println(graph.toDOT(SortedSet.empty[Int] ++ cycle.get.map(_.edgeID)))
}
