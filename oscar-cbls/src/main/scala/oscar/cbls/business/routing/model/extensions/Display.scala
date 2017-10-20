package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.visual.{RoutingMap, RoutingMapContainer}
import oscar.cbls.visual.ColorGenerator

/**
  * Created by fg on 18/09/17.
  */
class Display(vrp: VRP,
              nodePositions: List[(Double,Double)],
              displayOnRealMap: Boolean = false,
              selectRouteToDisplay: Boolean = false,
              sizeOfMap: Option[Int] = None,
              refreshRate: Int = 100
             ) {

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v),displayOnRealMap = displayOnRealMap, size = sizeOfMap)

  val RoutingMapContainer = new RoutingMapContainer(vrp, routingMap, title="Routing Map", refreshRate = refreshRate)

  def drawRoutes(): Unit ={
    RoutingMapContainer.refresh
  }

}
