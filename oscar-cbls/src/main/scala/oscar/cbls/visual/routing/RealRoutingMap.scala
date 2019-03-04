package oscar.cbls.visual.routing

import java.awt.Color

import org.jdesktop.swingx.mapviewer.{DefaultTileFactory, GeoPosition}
import oscar.cbls.business.routing._
import oscar.cbls.util.StopWatch
import oscar.visual.map._
import oscar.cbls._

import scala.swing.Color

/**
  * This class purpose is to display on a real map the resolution of a routing problem having
  * geographical coordinates for the nodes's location. In order to see the problem's resolution, the user
  * has to call the drawRoutes method after each validated movement. It is done in the search procedure.
  *   ex :
  *     (bestSlopeFirst(
  *       List(routeUnroutedPoint,
  *         onePtMove(10),
            customTwoOpt)
          ).afterMove(
            graphicExtension.drawRoutes())      <==== DrawRoute called after each (validated) move
  *
  * @param vrp The vehicle routing problem object.
  * @param geoCoords The array of geographical coordinates (of size n (one coordinate for each depot or customer))
  * @param colorValues The color of each vehicle (minimum size == v)
  * @param refreshRate The refresh rate in ms. For instance if its 1000. The display will update each second.
  */
class RealRoutingMap(vrp: VRP,
                     geoCoords: Array[(scala.Double,scala.Double)],
                     colorValues: Array[Color],
                     refreshRate: Long) extends VisualMap() with StopWatch with RoutingMapTrait {

  private var lastRefresh = 0L

  private lazy val depots:Array[MapWaypoint] = buildWaypoints()
  private lazy val customers:Array[MapPoint] = buildPoints()
  private lazy val roads:Array[MapLine] = buildLines()

  val tfRouting = new DefaultTileFactory(info)
  viewer.setTileFactory(tfRouting)
  viewer.setZoom(defineInitialZoom)
  viewer.setAddressLocation(centerOfMap())
  viewer.setName("Routing Map")
  viewer.setPreferredSize(new java.awt.Dimension(screensize.width / 2, screensize.height / 2))
  add(viewer)

  /**
    * Build all the depot location of the problem as MapWaypoint
    */
  private def buildWaypoints() ={
    Array.tabulate(vrp.v)(index => {
      val position = geoCoords(index)
      createWaypoint(position._1, position._2, colorValues(index))
    })
  }

  /**
    * Build all the customer location of the problem as MapPoint
    */
  private def buildPoints() ={
    Array.tabulate(vrp.n - vrp.v)(index => {
      val position = geoCoords(index + vrp.v)
      MapPoint(position._1,position._2)
    })
  }

  /**
    * Build all the lines required for this routing problem.
    * Each node of the problem has his line representing the route to his next node.
    * During the search we will simply move the destination of the node.
    * if dest == orig => the node isn't routed
    * @return an array of MapLine
    */
  private def buildLines(): Array[MapLine] ={
    Array.tabulate(vrp.n)(index => {
      val position = geoCoords(index)
      createLine((position._1,position._2),(position._1, position._2), Color.black)
    })
  }

  /**
    * @return the center of all nodes of the problem
    */
  private def centerOfMap(): GeoPosition ={
    val geoCoordsList = geoCoords.toList
    val latitudes = geoCoordsList.map(_._1)
    val longitudes = geoCoordsList.map(_._2)
    println(latitudes.min, latitudes.max, longitudes.min, longitudes.max)
    val latMiddle = (latitudes.max + latitudes.min)/2
    val lonMiddle = (longitudes.max + longitudes.min)/2
    println(latMiddle, lonMiddle)
    new GeoPosition(latMiddle, lonMiddle)
  }

  /**
    * This method compute the initial zoom needed to fit the node's location.
    * It's based on the nodes's location.
    * @return the initial zoom level
    */
  private def defineInitialZoom(): Int ={
    val positionsAtZoom1 = geoCoords.toList.map(g => tf.geoToPixel(new GeoPosition(g._1,g._2),1))
    val xs = positionsAtZoom1.map(_.getX)
    val ys = positionsAtZoom1.map(_.getY)
    val maxDist = Math.max(xs.max - xs.min,ys.max - ys.min)
    var zoom = 0
    while(maxDist/Math.pow(2,zoom) > 960)
      zoom += 1
    zoom+1
  }

  /**
    * Update the dest of each lines whose origin node is currently routed.
    * @param force Force the update
    */
  def drawRoutes(force: Boolean): Unit ={
    val currentTime = getWatch
    if(force || currentTime - lastRefresh >= refreshRate) {
      val routes = Array.tabulate(vrp.v)(v => vrp.getRouteOfVehicle(v))

      for (r <- 0 until vrp.v) {
        val color = colorValues(r)
        var previousPoint = routes(r).head
        for (p <- routes(r).drop(1)) {
          roads(longToInt(previousPoint)).color = color
          roads(longToInt(previousPoint)).dest = (customers(longToInt(p-vrp.v)).lat, customers(longToInt(p-vrp.v)).long)
          previousPoint = p
        }
        roads(longToInt(previousPoint)).color = color
        roads(longToInt(previousPoint)).dest = (depots(r).lat, depots(r).long)
      }
      lastRefresh = currentTime
    }
  }

}
