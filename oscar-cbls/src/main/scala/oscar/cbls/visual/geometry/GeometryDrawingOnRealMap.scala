package oscar.cbls.visual.geometry

import java.awt.Color

import org.jdesktop.swingx.mapviewer.{DefaultTileFactory, GeoPosition, TileFactoryInfo}
import org.locationtech.jts.geom.Geometry
import oscar.visual.map.VisualMap

class GeometryDrawingOnRealMap(pointOfOrigin: (Double,Double),
                               area: List[(Int, Int)],
                               geometryDrawing: SimpleGeometryDrawing) extends VisualMap() with GeometryDrawingTrait {

  val areaGeoCoords: List[GeoPosition] = area.map(p => convertDistToGeoCoords(p._1, p._2))

  val tfRouting = new DefaultTileFactory(info)
  viewer.setTileFactory(tfRouting)
  viewer.setZoom(defineInitialZoom)
  viewer.setAddressLocation(centerOfMap())
  viewer.setName("Routing Map")
  viewer.setPreferredSize(new java.awt.Dimension(screensize.width / 2, screensize.height / 2))
  add(viewer)

  //geometryDrawing.setOpaque(false)
  geometryDrawing.setBackground(new Color(0,0,0,0))
  add(geometryDrawing)

  val (minLatitude,maxLatitude,minLongitude,maxLongitude) =
    (areaGeoCoords.map(c => c.getLatitude).min,
      areaGeoCoords.map(c => c.getLatitude).max,
      areaGeoCoords.map(c => c.getLongitude).min,
      areaGeoCoords.map(c => c.getLongitude).max)

  drawAreaBorder()

  private def drawAreaBorder(): Unit ={
    var previousCoord: Option[GeoPosition] = None
    for (aCoord <- areaGeoCoords){
      if(previousCoord.isDefined)
        createLine((previousCoord.get.getLatitude,previousCoord.get.getLongitude),
          (aCoord.getLatitude, aCoord.getLongitude), Color.black)
      previousCoord = Some(aCoord)
    }
    if(previousCoord.isDefined)
      createLine((previousCoord.get.getLatitude,previousCoord.get.getLongitude),
        (areaGeoCoords.head.getLatitude,areaGeoCoords.head.getLongitude), Color.black)
  }

  /**
    * @return the center of the bounding box of the problem
    */
  private def centerOfMap(): GeoPosition ={
    new GeoPosition(
      maxLatitude - minLatitude,
      maxLongitude - minLongitude)
  }

  /**
    * This method compute the initial zoom needed to fit the are location
    * @return the initial zoom level
    */
  private def defineInitialZoom(): Int ={
    val positionsAtZoom1 = areaGeoCoords.map(g => tf.geoToPixel(g,1))
    val xs = positionsAtZoom1.map(_.getX)
    val ys = positionsAtZoom1.map(_.getY)
    val maxDist = Math.max(xs.max - xs.min,ys.max - ys.min)
    var zoom = 0
    while(maxDist/Math.pow(2,zoom) > 960)
      zoom += 1
    zoom+1
  }

  private def convertDistToGeoCoords(xFromOrigin: Int, yFromOrigin: Int): GeoPosition ={
    val oneLatDegreKM = 111.19
    val R = 6371

    val centerLat = (yFromOrigin.toDouble/(oneLatDegreKM*100000))+pointOfOrigin._1
    val centerLong = (xFromOrigin.toDouble/(R*Math.cos((centerLat+pointOfOrigin._1)/2)*100000))+pointOfOrigin._1
    new GeoPosition(centerLat, centerLong)
  }

  def drawShapes(boundingBoxOn:Option[Geometry] = None,shapes:List[(Geometry,Option[Color],Option[Color],String)],centers:List[(Int,Int)]): Unit ={
    println(geometryDrawing.getBackground)
    println(geometryDrawing.isOpaque)
    geometryDrawing.drawShapes(boundingBoxOn,shapes,centers)
  }
}

