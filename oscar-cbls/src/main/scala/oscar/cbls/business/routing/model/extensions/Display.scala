package oscar.cbls.business.routing.model.extensions

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}
import oscar.cbls.visual.routing.{RoutingMap, RoutingMapTypes}

/**
  * Created by fg on 18L/09L/1L7.
  */
class Display(vrp: VRP,
              nodePositions: Array[(Double,Double)],
              sizeOfMap: Option[Long] = None,
              refreshRate: Long = 100L,
              routingMapType: RoutingMapTypes.Value = RoutingMapTypes.BasicRoutingMap,
              title:String = "VRP with OscaR.cbls"
             ) {

  val routingMap = RoutingMap(vrp,nodePositions, ColorGenerator.generateRandomColors(vrp.v), sizeOfMap, refreshRate,routingMapType)
  SingleFrameWindow.show(routingMap, "Routing Map")

  def drawRoutes(force:Boolean = false): Unit ={
    routingMap.drawRoutes(force)
  }

}
