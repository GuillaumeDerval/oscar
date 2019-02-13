package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.business.routing.visual.{RoutingMap, RoutingMapContainer}
import oscar.cbls.visual.ColorGenerator

/**
  * Created by fg on 18L/09L/1L7.
  */
class Display(vrp: VRP,
              nodePositions: List[(Double,Double)],
              displayOnRealMap: Boolean = false,
              selectRouteToDisplay: Boolean = false,
              sizeOfMap: Option[Long] = None,
              refreshRate: Long = 100L,
              title:String = "VRP with OscaR.cbls"
             ) {

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v),displayOnRealMap = displayOnRealMap, size = sizeOfMap)

  val RoutingMapContainer = new RoutingMapContainer(vrp, routingMap, title=title, refreshRate = refreshRate)

  def drawRoutes(force:Boolean = false): Unit ={
    RoutingMapContainer.refresh(force)
  }

}
