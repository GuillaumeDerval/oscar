package oscar.cbls.business.routing.model.helpers

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.visual.routingMap.{RoutingMap, RoutingMapContainer}
import oscar.examples.cbls.routing.visual.ColorGenerator

/**
  * Created by fg on 18/09/17.
  */
class DisplayHelper(vrp: VRP,
                    nodePositions: List[(Double,Double)],
                    displayOnRealMap: Boolean = false,
                    selectRouteToDisplay: Boolean = false,
                    sizeOfMap: Option[Int] = None,
                    refreshRate: Int = 100
             ){

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v),displayOnRealMap = displayOnRealMap, size = sizeOfMap)

  val RoutingMapContainer = new RoutingMapContainer(title="Routing Map", vrp, routingMap, refreshRate = refreshRate)
  new Thread(RoutingMapContainer, "Routing display").start()

  def drawRoutes(): Unit ={
    RoutingMapContainer.setMustRefresh(true)
  }

}
