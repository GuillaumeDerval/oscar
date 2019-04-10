package oscar.cbls.test.vlsn

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, Edge}
import oscar.cbls.business.routing.neighborhood._
import oscar.cbls.business.routing.neighborhood.vlsn.{VLSNMoveType, _}
import oscar.cbls.business.routing.neighborhood.vlsn.CycleFinderAlgoType._
import oscar.cbls.business.routing.neighborhood.vlsn.VLSNMoveType.VLSNMoveType
import oscar.cbls.business.routing.neighborhood.vlsn.VLSNSNodeType.VLSNSNodeType
import oscar.cbls.core.search._
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.test.graph.RandomGraphGenerator
import oscar.cbls.test.graph.RandomGraphGenerator.generatePseudoPlanarConditionalGraph
import oscar.cbls.test.invariants.bench.{InvBench, ToMax}

import scala.util.Random

class MouthuyTestSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {


  /**
    * Will test wether a negative weight cycle is found in the graph.
    * If Mouthuy finds one, DFS must yield a cycle as well.
    * However, a cycle may not be found by Mouthuy, yet DFS finds one.
    */
  test("Mouthuy is coherent with DFS search (standard & pruned)") {
    forAll(graphGen) { vlsnGraph =>

      val mouthuyAlgo = CycleFinderAlgo(vlsnGraph, Mouthuy)
      val dfsAlgo = CycleFinderAlgo(vlsnGraph, DFS)
      val dfsAlgoPruned = CycleFinderAlgo(vlsnGraph, DFSPruned)

      val size = vlsnGraph.nodes.maxBy(_.nodeID).nodeID.toInt
      val liveNodes = Array.tabulate(size + 1)(_ => true)
      val cycle = mouthuyAlgo.findCycle(liveNodes)

      if (cycle.isDefined) {
        dfsAlgo.findCycle(liveNodes) shouldBe defined
        dfsAlgoPruned.findCycle(liveNodes) shouldBe defined
        println("Mouthuy found a cycle")
      }
      else if (dfsAlgo.findCycle(liveNodes).isDefined) {
        println("DFS found a cycle")
      }
      else if (dfsAlgoPruned.findCycle(liveNodes).isDefined) {
        println("DFS pruned found a cycle")
      }
      else {
        println("Definitely no cycle in this graph.")
      }
    }
  }

  test("Mouthuy with deterministic values"){

    val nbNodes = 6
    val nodes = Array.tabulate(nbNodes)(nodeID  =>
      new Node(nodeID, nbNodes + nodeID,VLSNSNodeType.RegularNode,nodeID,nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[Node], nbNodes,2) //nbLAbel is set here to nbNodes

    def edge(from: Long, to: Long, gain: Long): Unit = {
      builder.addEdge(
        nodes(from.toInt),
        nodes(to.toInt),
        gain,
        moveTypeObjectGen.sample.get,
        moveTypeEnumGen.sample.get
      )
    }

    edge(0L, 1L, 1L)
    edge(1L, 2L, 1L)
    edge(2L, 3L, 1L)
    edge(3L, 4L, 1L)
    edge(4L, 0L, -1L)
    edge(0L, 3L, 1L)
    edge(2L, 4L, -2L)
    edge(2L, 5L, 2L)
    edge(5L, 0L, 1L)
    edge(4L, 2L, -1L)

    val liveNodes = Array.tabulate(6)(_ => true)
    val graph = builder.finish()
    val mouthuyAlgo = CycleFinderAlgo(graph, Mouthuy)
    val cycle = mouthuyAlgo.findCycle(liveNodes)

    println(graph)

    cycle shouldBe defined
  }



  val bench = new InvBench(0,List(ToMax()))
  val moveTypeEnumGen :Gen[VLSNMoveType.Value] = Gen.frequency(
    (10, VLSNMoveType.InsertNoEject),
    (10, VLSNMoveType.InsertWithEject),
    (10, VLSNMoveType.MoveNoEject),
    (10, VLSNMoveType.MoveWithEject),
    (10, VLSNMoveType.Remove),
    (10, VLSNMoveType.SymbolicTrashToInsert),
    (10, VLSNMoveType.SymbolicVehicleToTrash),
    (10, VLSNMoveType.SymbolicTrashToNodeForEject)
  )

  val moveTypeObjectGen :Gen[Move] = Gen.frequency(
    (1,AddToSetMove(bench.genIntSetVar(5),0L,0L)),
    (1,AssignMove(bench.genIntVar(0 to 100),0L,0,0L)),
    (1,CallBackMove(() => {},0L,"")),
    (1,CompositeMove(List(),0L)),
    (1,DoNothingMove(0L)),
    (1,FlipMove(0L,1L,bench.genIntVars().toArray,0L)),
    (1,GradientMove(List(),0L,0L)),
    (1,InsertPointMove(0L,0L,0L,true,0L,null,null)),
    (1,InstrumentedMove(new DoNothingMove(0L))),
    (1,LoadSolutionMove(null,0L)),
    (1,NamedMove(new DoNothingMove(0L))),
    (1,RemoveFromSetMove(bench.genIntSetVar(),0L,0L)),
    (1,RollMove(bench.genIntVars(),0L,0L)),
    (1,ShiftMove(0L,0L,0L,null,0L)),
    (1,SwapMove(null,null,0,0,0))
  )

  val nodeTypeGen :Gen[VLSNSNodeType.Value] = Gen.frequency(
    (10,VLSNSNodeType.RegularNode),
    (10,VLSNSNodeType.VehicleNode),
    (2,VLSNSNodeType.UnroutedNode),
    (1,VLSNSNodeType.FictiveNode),
  )

  val graphGen: Gen[VLSNGraph] = {

    val nbNodes = Gen.choose(10, 100).sample.get
    val nbEdges = Gen.choose(nbNodes * 2, nbNodes * 3).sample.get
    val tempGraph = generatePseudoPlanarConditionalGraph(nbNodes, 0, nbEdges, 0)

    val nodes = Array.tabulate(nbNodes)(nodeID =>
      new oscar.cbls.business.routing.neighborhood.vlsn.Node(nodeID, nbNodes + nodeID, nodeTypeGen.sample.get, nodeID, nodeID))

    val builder = new VLSNEdgeBuilder(nodes: Array[oscar.cbls.business.routing.neighborhood.vlsn.Node], nbNodes, 2) //nbLAbel is set here to nbNodes

    for (tempEdge <- tempGraph.edges) {
      val (from,to) = if(Random.nextBoolean()) (tempEdge.nodeIDA,tempEdge.nodeIDB) else (tempEdge.nodeIDB,tempEdge.nodeIDA)
      builder.addEdge(
        nodes(from),
        nodes(to),
        Gen.choose(-1000, 10).sample.get,
        moveTypeObjectGen.sample.get,
        moveTypeEnumGen.sample.get)
    }

    builder.finish()
  }
}



