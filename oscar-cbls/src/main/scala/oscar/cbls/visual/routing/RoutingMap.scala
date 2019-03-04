package oscar.cbls.visual.routing

import java.awt.Color

import javax.swing.JPanel
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.util.StopWatch
import oscar.visual.VisualDrawing

trait RoutingMapTrait{
  def drawRoutes(force: Boolean)
}

object RoutingMapTypes extends Enumeration{
  val BasicRoutingMap, RealRoutingMap = Value
}

object RoutingMap{

  def apply(vrp: VRP,
            nodesPositions: Array[(scala.Double,scala.Double)],
            vehiclesToColor: Array[Color],
            size: Option[Long],
            resfreshRate: Long,
            routingMapType: RoutingMapTypes.Value): JPanel with RoutingMapTrait ={
    routingMapType match{
      case RoutingMapTypes.BasicRoutingMap =>
        new BasicRoutingMap(vrp,nodesPositions,vehiclesToColor,size,resfreshRate)
      case RoutingMapTypes.RealRoutingMap =>
        new RealRoutingMap(vrp,nodesPositions,vehiclesToColor,resfreshRate)
    }
  }
}
