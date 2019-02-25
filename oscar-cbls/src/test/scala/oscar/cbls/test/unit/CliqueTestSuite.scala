package oscar.cbls.test.unit

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.clique.Clique
import oscar.cbls.algo.magicArray.MagicBoolArray

class CliqueTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  //First test with madeup results
  test("Reported clique is expected"){
    val nbNodes = 10
    val adjacencyList:List[(Long,Long)] = List((1,2),(1,3),(4,2),(3,4),(3,6),(6,5),(4,5),(3,5),(4,6),(6,8),(8,5),(5,7))

    val adjacencyDico = adjacencyList ++ adjacencyList.map{case (a,b) => (b,a)}
    def isNeighbor(a:Long,b:Long) = adjacencyDico.contains((a,b))

    val cliques = Clique.bronKerbosch2(nbNodes,isNeighbor:(Long,Long)=>Boolean)

    // Should be only one clique of size 4
    cliques.count(set => set.size == 4) should be (1)

    // This clique should be (3,6,4,5)
    val maxclique = cliques.filter(set => set.size == 4).head
    List(3L,6,4,5).forall(maxclique.contains)
  }

  val nbNodes = for (n <- Gen.choose(0, 100)) yield n
  val nodeList = Gen.listOf()


  test("Reported clique is strongly connected"){
    forAll(nodeList){list => {

      val array = MagicBoolArray(list.size, initVal = true)
      for ((elem,i) <- list.view.zipWithIndex) {
        array.update(i,elem == 1)
      }

      for ((elem,i) <- list.view.zipWithIndex) {
        array(i) should be (elem == 1)
      }
    }}
  }
}
