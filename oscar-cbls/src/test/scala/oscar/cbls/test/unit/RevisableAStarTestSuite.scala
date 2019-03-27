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

  test("Flipping from and to on search yield the same value (and possibly the same path)"){

    val nbNodes = 60
    val nbConditionalEdges = 110
    val nbNonConditionalEdges = 50
    val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(nbNodes,
      nbConditionalEdges,
      nbNonConditionalEdges,
      nbTransitNodes = nbNodes / 2,
      mapSide = 1000)

    val open = Array.tabulate((nbConditionalEdges * 0.7).toInt)(_ => 1).toList
    val closed = Array.tabulate((nbConditionalEdges * 0.3).toInt)(_ => 0).toList
    val openConditions = Random.shuffle(open ::: closed)

    val underApproxDistanceMatrix = FloydWarshall.buildDistanceMatrix(graph, _ => true)
    val aStar = new RevisableAStar(graph, underApproximatingDistance = (a: Int, b: Int) => underApproxDistanceMatrix(a)(b))

    for (nodeFrom <- graph.nodes) {
      for (nodeTo <- graph.nodes) {

        val res1 = aStar.search(nodeFrom, nodeTo, id => openConditions(id) == 1, includePath = true)
        val res2 = aStar.search(nodeTo, nodeFrom, id => openConditions(id) == 1, includePath = true)
        (res1, res2) match {
          case (Distance(_, _, dist1, conditions1, _, path1), Distance(_, _, dist2, conditions2, _, path2)) =>
            dist1 should be(dist2)
            if (path1.isDefined) {

              path2 should not be None

              val pathForward = path1.get
              val pathBackward = path2.get
              val conditionsForward = pathForward.filter(_.conditionID.isDefined).sortBy(_.conditionID.get)
              val conditionsBackward = pathBackward.filter(_.conditionID.isDefined).sortBy(_.conditionID.get)
              if (pathForward.sortBy(_.id) equals pathBackward.sortBy(_.id)) {
                // If the path is the same, the conditions should be the same
                conditionsForward should be(conditionsBackward)
              }
            }

          case _ =>
        }
      }
    }
  }

  def exportGraphToNetworkxInstructions(graph :ConditionalGraphWithIntegerNodeCoordinates, openConditions :List[Int]): String ={

    var toReturn = s"nbNodes = ${graph.nbNodes}\n"

    val nonConditionalEdges = graph.edges.filter(e => e.conditionID.isEmpty).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val openEdges =  graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 1)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val closeEdges = graph.edges.filter(e => e.conditionID.isDefined && (openConditions(e.conditionID.get) == 0)).map(e => s"(${e.nodeIDA},${e.nodeIDB})").mkString(",")
    val nodesPositions = graph.coordinates.zipWithIndex.map({case (e,i) => s"$i : (${e._1},${e._2})"}).mkString(",")

    toReturn = toReturn.concat(s"openEdges = [$openEdges]\n")
    toReturn = toReturn.concat(s"closedEdges = [$closeEdges]\n")
    toReturn = toReturn.concat(s"nonConditionalEdges = [$nonConditionalEdges]\n")
    toReturn = toReturn.concat(s"pos = {${nodesPositions}}")

    toReturn
  }
}
