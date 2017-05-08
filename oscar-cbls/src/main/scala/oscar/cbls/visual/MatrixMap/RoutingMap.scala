package oscar.cbls.visual.MatrixMap

/**
  * *****************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  * ****************************************************************************
  */

import java.awt.event.{MouseEvent, MouseListener}
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D.Double
import java.awt.{BasicStroke, Color, Graphics2D, Point, Rectangle, RenderingHints}

import org.jxmapviewer.painter.Painter
import org.jxmapviewer.viewer.GeoPosition
import org.jxmapviewer.{JXMapKit, JXMapViewer}
import oscar.cbls.business.routing.model.{PDP, VRP}
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualArrow, VisualCircle, VisualLine, VisualShape}

import scala.collection.mutable.ListBuffer

/**
  * @author fabian.germeau@student.vinci.be
  */

trait RoutingMap{

  var pointsList:scala.List[(Int, Int)] = List.empty
  var colorValues:Array[Color] = Array.empty
  var vrp:VRP = _
  var pdp:PDP = _
  var mapSize = 10000
  var routesToDraw:Array[List[Int]] = Array.empty

  def drawPoints()

  def drawRoutes()

  def setColorValues(colorValues:Array[Color]): Unit ={
    if(colorValues == null){
      this.colorValues = ColorGenerator.generateRandomColors(vrp.v)
    }else{
      this.colorValues = colorValues
    }
  }

  def setPointsList(pointsList:scala.List[(scala.Double,scala.Double)])

  def setMapSize(mapSize:Int)

  protected  def setPointsInformation()

  def setRouteToDisplay(rtd:Array[List[Int]]): Unit ={
    routesToDraw = rtd
    drawRoutes()
  }

  def setVRP(vrp:VRP): Unit ={
    this.vrp = vrp
  }

  def setPDP(pdp:PDP): Unit ={
    this.pdp = pdp
    this.vrp = pdp
    setPointsInformation()
  }
}

class BasicRoutingMap(pdpOption:Option[PDP], vrpOption:Option[VRP]) extends VisualDrawing(false,false) with RoutingMap{
  pdpOption.getOrElse(vrpOption.get) match{
    case p:PDP => setPDP(p)
    case v:VRP => setVRP(v)
  }

  val points:Array[VisualCircle] = new Array[VisualCircle](vrp.n)

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def drawPoints() ={
    var index = 0
    for(p <- pointsList){
      if(index < vrp.v){
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,3)
        tempPoint.innerCol_$eq(colorValues(index))
        points(index) = tempPoint
      }
      else{
        val tempPoint = new VisualCircle(this,p._1.toInt,p._2.toInt,1)
        tempPoint.innerCol_$eq(Color.black)
        points(index) = tempPoint
      }
      index += 1
    }
  }

  def drawRoutes(): Unit ={
    clear()
    drawPoints()

    var previousPoint = -1
    var color:Color = null
    for(r <- routesToDraw.indices) {
      color = colorValues(r)
      for (p <- routesToDraw(r)) {
        if(previousPoint >= 0){
          val tempRoute =
            if(vrp.n <= 10)
              new VisualArrow(this, new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(p)._1, pointsList(p)._2), 4)
            else
              new VisualLine(this,new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(p)._1, pointsList(p)._2))
          tempRoute.outerCol_$eq(color)
          tempRoute.borderWidth = 2
        }
        previousPoint = p
      }
      val tempRoute =
        if(vrp.n <= 10)
          new VisualArrow(this, new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(r)._1, pointsList(r)._2), 4)
        else
          new VisualLine(this,new Double(pointsList(previousPoint)._1, pointsList(previousPoint)._2, pointsList(r)._1, pointsList(r)._2))
      tempRoute.outerCol_$eq(color)
      tempRoute.borderWidth = 2
      previousPoint = -1
    }
  }

  def clear(): Unit ={
    super.clear()
  }

  override def setPointsList(pointsList: List[(scala.Double, scala.Double)]): Unit = {
    val tempList:ListBuffer[(Int,Int)] = new ListBuffer[(Int,Int)]
    for(p <- pointsList){
      val tempP = (p._1.toInt*getHeight/mapSize, p._2.toInt*getHeight/mapSize)
      tempList.append(tempP)
    }
    this.pointsList = tempList.toList
  }

  override def setMapSize(mapSize: Int): Unit = {
    this.mapSize = mapSize
  }

  override def setPointsInformation(): Unit ={
    require(pdp != null, "In order to use the RouteToDisplay trait you must specify a VRP object.")

    for(p <- points.indices){
      points(p).toolTip_=(getPointInformation(p))
    }

    def getPointInformation(p:Int): String ={
      "Some informations about the point : " + p + "\n" +
        "Arrival time : " + pdp.arrivalTimes(p).value + "\n" +
        "Leave time : " + pdp.leaveTimes(p).value + "\n" +
        "Passengers on board : " + pdp.arrivalLoadValue(p).value
    }
  }
}


class GeoRoutingMap extends JXMapKit with RoutingMap {
  setDefaultProvider(JXMapKit.DefaultProviders.OpenStreetMaps)
  setZoomButtonsVisible(false)
  setZoomSliderVisible(false)
  setMiniMapVisible(false)
  setZoom(7)
  var zoomChanged = false
  var geoCoords:List[(scala.Double,scala.Double)] = List.empty

  def geoCoordToPixel(): Unit ={
    val tempList:ListBuffer[(Int,Int)] = new ListBuffer[(Int,Int)]
    for(c <- geoCoords){
      val geoPosition = new GeoPosition(c._1, c._2)
      val pixelPosition = getMainMap.getTileFactory.geoToPixel(geoPosition, getMainMap.getZoom)
      tempList.append((pixelPosition.getX.toInt,pixelPosition.getY.toInt))
    }
    pointsList = tempList.toList
  }

  override def drawPoints(): Unit = {
    val painter = new Painter[JXMapViewer]{
      override def paint(g : Graphics2D, t : JXMapViewer, i: Int, i1: Int): Unit = {
        val rect:Rectangle = getMainMap.getViewportBounds
        g.translate(-rect.x, -rect.y)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setStroke(new BasicStroke(2))

        for(p <- pointsList)
          g.draw(new Ellipse2D.Double(p._1, p._2, 2, 2))
      }
    }
    getMainMap.setOverlayPainter(painter)
  }

  override def drawRoutes(): Unit = {
    val painter = new Painter[JXMapViewer]{
      override def paint(g: Graphics2D, t: JXMapViewer, i: Int, i1: Int): Unit = {
        val rect:Rectangle = getMainMap.getViewportBounds
        g.translate(-rect.x, -rect.y)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setStroke(new BasicStroke(2))

        //If the zoom has changed we must recalculate the position of each point
        if(zoomChanged)
          geoCoordToPixel()

        /**
          * This method draw a complete arrow which represents the distance between two points.
          * @param x1 the x value of the "from" point
          * @param y1 the y value of the "from" point
          * @param x2 the x value of the "to" point
          * @param y2 the y value of the "to" point
          */
        def drawArrow(x1:Int,y1:Int,x2:Int,y2:Int): Unit ={
          g.drawLine(x1, y1, x2, y2)

          var degree = Math.asin((y1 - y2)/Math.sqrt(Math.pow(y1 - y2,2) + Math.pow(x1 - x2,2)))
          if(x1 - x2 < 0){
            degree = 3.14159 - degree
          }

          val x3 = (Math.cos(degree + 0.3926991)*10 + x2).toInt
          val y3 = (Math.sin(degree + 0.3926991)*10 + y2).toInt
          val x4 = (Math.cos(degree - 0.3926991)*10 + x2).toInt
          val y4 = (Math.sin(degree - 0.3926991)*10 + y2).toInt

          g.drawLine(x2, y2, x3, y3)
          g.drawLine(x2, y2, x4, y4)


        }

        g.setColor(Color.black)
        for(p <- pointsList)
          g.draw(new Ellipse2D.Double(p._1, p._2, 2, 2))

        var from = 0
        for(vehicle <- routesToDraw.indices) {
          val route = routesToDraw(vehicle)
          if (route.length > 1) {
            g.setColor(colorValues(vehicle))
            for (node <- route) {
              if (node > 0 && pointsList(from) != pointsList(node))
                drawArrow(pointsList(from)._1, pointsList(from)._2, pointsList(node)._1, pointsList(node)._2)
              from = node
            }
            if(pointsList(from) != pointsList(vehicle))
              drawArrow(pointsList(from)._1, pointsList(from)._2, pointsList(vehicle)._1, pointsList(vehicle)._2)
          }
        }
      }
    }
    getMainMap.setOverlayPainter(painter)
  }

  override def setPointsList(pointsList: List[(scala.Double, scala.Double)]): Unit = {
    val rect:Rectangle = getMainMap.getViewportBounds
    geoCoords = pointsList
    val tempList:ListBuffer[(Int,Int)] = new ListBuffer[(Int,Int)]
    var latSum = 0.0
    var lonSum = 0.0
    for(p <- pointsList){
      val geoPosition = new GeoPosition(p._1, p._2)
      val pixelPosition = getMainMap.getTileFactory.geoToPixel(geoPosition, getMainMap.getZoom)
      tempList.append((pixelPosition.getX.toInt,pixelPosition.getY.toInt))
      latSum += p._1
      lonSum += p._2
    }
    this.pointsList = tempList.toList
    setAddressLocation(new GeoPosition(latSum/pointsList.size, lonSum/pointsList.size))
  }

  override def setMapSize(mapSize: Int): Unit = {}

  override def setZoom(zoom: Int): Unit = {
    zoomChanged = true
    super.setZoom(zoom)
  }

  override def setPointsInformation(): Unit ={
    require(pdp != null, "In order to use the GeoPickupAndDeliveryPoints trait you must specify a PDP object.")

    getMainMap.addMouseListener(new MouseListener {
      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {
        val rect = getMainMap.getViewportBounds
        for(i <- pointsList.indices){
          val point = new Point(pointsList(i)._1.toInt - rect.x, pointsList(i)._2.toInt - rect.y)
          if(point.distance(e.getPoint) < 10) {
            println("Some informations about the point : " + i)
            println("Arrival time : " + pdp.arrivalTimes(i).value)
            println("Leave time : " + pdp.leaveTimes(i).value)
            println("Passengers on board : " + pdp.arrivalLoadValue(i).value)
            println()
          }
        }
        println()
      }

      override def mouseEntered(e: MouseEvent): Unit = {}

      override def mousePressed(e: MouseEvent): Unit = {}

      override def mouseReleased(e: MouseEvent): Unit = {}
    })
  }
}
