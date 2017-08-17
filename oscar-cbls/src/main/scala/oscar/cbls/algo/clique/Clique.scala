package oscar.cbls.algo.clique

import scala.collection.immutable.SortedSet

/**
 * Created by rdl on 17-08-17.
 */
object Clique {

  def bronKerbosch2(nbNodes:Int,isNeighbor:(Int,Int)=>Boolean):List[SortedSet[Int]] = {

    var allCliques:List[SortedSet[Int]] = List.empty
    def addMaximalClique(clique:SortedSet[Int]) = allCliques = clique::allCliques

    def allNodes = 0 to nbNodes

    def bronKerbosch2Search(r:SortedSet[Int],p0:SortedSet[Int],x0:SortedSet[Int]) {
      //  BronKerbosch2(R,P,X):
      //  if P and X are both empty:
      if (p0.isEmpty && x0.isEmpty) {
        //    report R as a maximal clique
        addMaximalClique(r)
      } else {
        var p : SortedSet[Int] = p0
        var x : SortedSet[Int] = x0

        //    choose a pivot vertex u in P ⋃ X
        val u = if (p.nonEmpty) p.head else x.head
        //    for each vertex v in P \ N(u):

        for (v <- p if !isNeighbor(v, u)) {
          val allNeighborsOfV = allNodes.filter(isNeighbor(v, _))

          //      BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          bronKerbosch2Search(r + v,
            SortedSet.empty[Int] ++ allNeighborsOfV.filter(p contains _),
            SortedSet.empty[Int] ++ allNeighborsOfV.filter(x contains _))
          //    P := P \ {v}
          p = p - v
          //    X := X ⋃ {v}
          x = x + v
        }
      }
    }
    bronKerbosch2Search(SortedSet.empty[Int],SortedSet.empty[Int] ++ allNodes,SortedSet.empty[Int])

    allCliques
  }
}
