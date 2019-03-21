package oscar.cbls.test.unit

import org.scalacheck.Gen
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import oscar.cbls.CBLSSetVar
import oscar.cbls.algo.graph._
import oscar.cbls.test.invariants.bench.{InvBench, ToZero}

import scala.util.Random

class RevisableAStarTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val verbose = 0

  test("search from node to itself should be Distance(0)"){

    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 9
    val nbNonConditionalEdges = 5
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graph.nodes)

    forAll(gen){
      node =>
        val result = aStar.search(node,node,_ => true,false)
        result should be (a[Distance])
        result match{
          case Distance(_, _, distance1, _, _, _) =>
            distance1 should be (0)
        }
    }
  }

  test("search from node to unreachable node should be NeverConnected"){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val openConditions:CBLSSetVar = bench.genIntSetVar(nbVars = 0, range = 0 to 0)
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges,
      nbConditions = graphTemp.nbConditions)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graphTemp.nodes)

    forAll(gen){
      node =>
        val result = aStar.search(node,lonelyNode,_ => true,false)
        result should be (a[NeverConnected])
    }
  }

  test("search from node to disconnected node should be NotConnected or NeverConnected "){
    val bench = new InvBench(verbose,List(ToZero()))
    val nbNodes = 10
    val nbConditionalEdges = 0
    val nbNonConditionalEdges = 15
    val graphTemp = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val lonelyNode = new Node(nbNodes,true)
    val closedEdgeToLonelyNode = new Edge(nbNonConditionalEdges,lonelyNode,graphTemp.nodes(0),50,Some(0))

    val graph = new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = graphTemp.coordinates :+ (0,0),
      nodes = graphTemp.nodes :+ lonelyNode,
      edges = graphTemp.edges :+ closedEdgeToLonelyNode,
      nbConditions = graphTemp.nbConditions+1)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => false)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.oneOf(graphTemp.nodes)


    forAll(gen){
      node =>
        val result = aStar.search(node,lonelyNode,_ => true,false)
        result should (be (a[NotConnected]) or be (a[NeverConnected]))
    }
  }

  test("path should contain only non-conditional edges or open conditional edges"){

    val nbNodes = 10
    val nbConditionalEdges = 9
    val nbNonConditionalEdges = 5
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes/2,
      mapSide = 1000)

    val openConditions = Random.shuffle(List(0,0,0,0,1,1,1,1,1))
    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph,_ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a:Int,b:Int) => underApproxDistanceMatrix(a)(b))
    val gen = Gen.listOfN(2,Gen.oneOf(graph.nodes))

    forAll(gen){
      nodesCouple =>
        whenever(nodesCouple.size == 2 && nodesCouple.head != nodesCouple(1)){
          val nodeFrom = nodesCouple.head
          val nodeTo = nodesCouple(1)
          val result = aStar.search(nodeFrom,nodeTo,id => openConditions(id) == 1,includePath = true)
          result match {
            case Distance(_,_,_,_,_,path) =>
              path.get.foreach(e =>
                e.conditionID match{
                  case Some(c) => openConditions(c) should be (1)
                  case None =>
                }
              )
            case _ =>
          }
        }
    }
  }
}
