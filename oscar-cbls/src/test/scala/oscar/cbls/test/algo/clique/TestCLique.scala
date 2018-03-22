package oscar.cbls.test.algo.clique

import oscar.cbls.algo.clique.Clique

import scala.collection.immutable.SortedSet


/**
  * Testing object
  */object TestClique extends App{

  val nbNodes = 10
  val adjacencyList:List[(Int,Int)] = List((0,2),(2,8),(3,1),(1,4),(3,4),(7,5),(3,6),(4,8))

  val adjacencyDico = SortedSet.empty[(Int,Int)] ++ adjacencyList ++ adjacencyList.map{case (a,b) => (b,a)}
  def isNeighbor(a:Int,b:Int) = adjacencyDico.contains((a,b))

  val cliques = Clique.bronKerbosch2(nbNodes,isNeighbor:(Int,Int)=>Boolean).map(_.toList)
  println(cliques)

  require(cliques equals List(List(9), List(2, 8), List(5, 7), List(4, 8), List(3, 6), List(1, 3, 4), List(0, 2)))
}