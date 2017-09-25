package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.visual.MatrixMap.{RoutingMap, RoutingMapContainer}
import oscar.examples.cbls.routing.visual.ColorGenerator

import scala.collection.immutable.HashSet

/**
  * Created by fg on 18/09/17.
  */
class Display(vrp: VRP,
              nodePositions: List[(Double,Double)],
              displayOnRealMap: Boolean = false,
              selectRouteToDisplay: Boolean = false,
              sizeOfMap: Option[Int] = None,
              refreshRate: Int = 100
             ) extends VRPExtension(vrp) {

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v),displayOnRealMap = displayOnRealMap, size = sizeOfMap)

  val RoutingMapContainer = new RoutingMapContainer(title="Routing Map", vrp, routingMap, refreshRate = refreshRate)
  new Thread(RoutingMapContainer, "Routing display").start()

  def drawRoutes(): Unit ={
    RoutingMapContainer.setMustRefresh(true)
  }

  override def preComputeRelevantNeighborsOfNode(node: Int, potentialRelevantNeighbors: List[Int]): List[Int] = {
    potentialRelevantNeighbors
  }

  override def postFilter(node: Int) = (neighbor: Int) => true

}
