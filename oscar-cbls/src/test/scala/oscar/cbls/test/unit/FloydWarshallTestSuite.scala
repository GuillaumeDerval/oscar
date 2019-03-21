package oscar.cbls.test.unit

import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.CBLSSetVar
import oscar.cbls.algo.graph.{FloydWarshall, RandomGraphGenerator}

class FloydWarshallTestSuite extends FunSuite with Matchers{

  test("diagonal of matrix from Floyd Warshall should be 0"){
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    for(i <- underApproxDistanceMatrix.indices){
      underApproxDistanceMatrix(i)(i) should be (0)
    }
  }

  test("everything but diagonal of matrix from Floyd Warshall should be != 0"){
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15

    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    for(i <- underApproxDistanceMatrix.indices){
      for(j <- underApproxDistanceMatrix.indices){
        if(i != j){
          underApproxDistanceMatrix(i)(j) should not be 0
        }
      }
    }
  }
}
