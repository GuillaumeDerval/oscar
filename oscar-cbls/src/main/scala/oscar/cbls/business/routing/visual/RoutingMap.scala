package oscar.cbls.business.routing.visual

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
import javax.swing.JPanel

import org.jxmapviewer.painter.Painter
import org.jxmapviewer.viewer.GeoPosition
import org.jxmapviewer.{JXMapKit, JXMapViewer}
import oscar.cbls.algo.quick.QList
import oscar.cbls.business.routing.model.VRP
import oscar.visual.VisualDrawing
import oscar.visual.shapes.{VisualArrow, VisualCircle, VisualLine, VisualShape}

/**
  * @author fabian.germeau@student.vinci.be
  */

class BasicRoutingMap(vrp: VRP,
                      nodesPositions: List[(scala.Double,scala.Double)],
                      colorValues: Array[Color],
                      val mapSize: Int) extends VisualDrawing(false,false) with RoutingMapDisplay{

  val points:Array[VisualCircle] = new Array[VisualCircle](vrp.n)
  lazy val pixelPositionOfNodes: List[(scala.Double,scala.Double)] = positionsToPixels()

  override def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit ={
    super.addShape(shape,false)
  }

  def drawPoints() ={
    var index = 0
    for(p <- pixelPositionOfNodes){
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

    val routes = Array.tabulate(vrp.v)(vrp.getRouteOfVehicle)

    var previousPoint = -1
    var color:Color = null
    for(r <- 0 until vrp.v) {
      color = colorValues(r)
      for (p <- routes(r)) {
        if(previousPoint >= 0){
          val tempRoute =
            if(vrp.n <= 10)
              new VisualArrow(this, new Double(pixelPositionOfNodes(previousPoint)._1, pixelPositionOfNodes(previousPoint)._2, pixelPositionOfNodes(p)._1, pixelPositionOfNodes(p)._2), 4)
            else
              new VisualLine(this,new Double(pixelPositionOfNodes(previousPoint)._1, pixelPositionOfNodes(previousPoint)._2, pixelPositionOfNodes(p)._1, pixelPositionOfNodes(p)._2))
          tempRoute.outerCol_$eq(color)
          tempRoute.borderWidth = 2
        }
        previousPoint = p
      }
      val tempRoute =
        if(vrp.n <= 10)
          new VisualArrow(this, new Double(pixelPositionOfNodes(previousPoint)._1, pixelPositionOfNodes(previousPoint)._2, pixelPositionOfNodes(r)._1, pixelPositionOfNodes(r)._2), 4)
        else
          new VisualLine(this,new Double(pixelPositionOfNodes(previousPoint)._1, pixelPositionOfNodes(previousPoint)._2, pixelPositionOfNodes(r)._1, pixelPositionOfNodes(r)._2))
      tempRoute.outerCol_$eq(color)
      tempRoute.borderWidth = 2
      previousPoint = -1
    }
  }

  def clear(): Unit ={
    super.clear()
  }

  def positionsToPixels(): List[(scala.Double, scala.Double)] = {
    var pixelPositionsOfNodes:QList[(scala.Double,scala.Double)] = null
    for(p <- nodesPositions.reverse){
      val pixelPositionOfNode = (p._1*getHeight/mapSize, p._2*getHeight/mapSize)
      pixelPositionsOfNodes = QList(pixelPositionOfNode,pixelPositionsOfNodes)
    }
    pixelPositionsOfNodes.toList
  }

  def setPointsInformation(popupPointInformation: (Int) => String): Unit ={
    for(p <- points.indices){
      points(p).toolTip_=(popupPointInformation(p))
    }
  }
}


class GeoRoutingMap(vrp: VRP, geoCoords:List[(scala.Double, scala.Double)], vehicleToColor: Array[Color]) extends JXMapKit with RoutingMapDisplay {
  var pixelPositionsOfNodes: List[(Int,Int)] = geoCoordsToPixels()
  geoCoordsToPixels()

  setDefaultProvider(JXMapKit.DefaultProviders.OpenStreetMaps)
  setZoomButtonsVisible(false)
  setZoomSliderVisible(false)
  setMiniMapVisible(false)
  setZoom(7)
  setAddressLocation(
    new GeoPosition(pixelPositionsOfNodes.map(_._1).sum/vrp.n,
      pixelPositionsOfNodes.map(_._2).sum/vrp.n))

  def geoCoordsToPixels(): List[(Int,Int)] ={
    var pixelPositionsOfNodes: QList[(Int,Int)] = null
    for(nodeCoord <- geoCoords.reverse){
      val geoPosition = new GeoPosition(nodeCoord._1, nodeCoord._2)
      val pixelPosition = getMainMap.getTileFactory.geoToPixel(geoPosition, getMainMap.getZoom)
      pixelPositionsOfNodes = QList((pixelPosition.getX.toInt, pixelPosition.getY.toInt),pixelPositionsOfNodes)
    }
    pixelPositionsOfNodes.toList
  }

  def drawPoints(): Unit = {
    val painter = new Painter[JXMapViewer]{
      override def paint(g : Graphics2D, t : JXMapViewer, i: Int, i1: Int): Unit = {
        val rect:Rectangle = getMainMap.getViewportBounds
        g.translate(-rect.x, -rect.y)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setStroke(new BasicStroke(2))

        for(p <- pixelPositionsOfNodes)
          g.draw(new Ellipse2D.Double(p._1, p._2, 2, 2))
      }
    }
    getMainMap.setOverlayPainter(painter)
  }

  def drawRoutes(): Unit = {
    val routes = Array.tabulate(vrp.v)(vrp.getRouteOfVehicle)

    val painter = new Painter[JXMapViewer]{
      override def paint(g: Graphics2D, t: JXMapViewer, i: Int, i1: Int): Unit = {
        val rect:Rectangle = getMainMap.getViewportBounds
        g.translate(-rect.x, -rect.y)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.setStroke(new BasicStroke(2))

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
        for(p <- pixelPositionsOfNodes)
          g.draw(new Ellipse2D.Double(p._1, p._2, 2, 2))

        var from = 0
        for(vehicle <- 0 until vrp.v) {
          val route = routes(vehicle)
          if (route.length > 1) {
            g.setColor(vehicleToColor(vehicle))
            for (node <- route) {
              if (node > 0 && pixelPositionsOfNodes(from) != pixelPositionsOfNodes(node))
                drawArrow(pixelPositionsOfNodes(from)._1, pixelPositionsOfNodes(from)._2, pixelPositionsOfNodes(node)._1, pixelPositionsOfNodes(node)._2)
              from = node
            }
            if(pixelPositionsOfNodes(from) != pixelPositionsOfNodes(vehicle))
              drawArrow(pixelPositionsOfNodes(from)._1, pixelPositionsOfNodes(from)._2, pixelPositionsOfNodes(vehicle)._1, pixelPositionsOfNodes(vehicle)._2)
          }
        }
      }
    }
    getMainMap.setOverlayPainter(painter)
  }



  override def setZoom(zoom: Int): Unit = {
    geoCoordsToPixels()
    super.setZoom(zoom)
  }

  def setPointsInformation(popupPointInformation: (Int) => String): Unit ={

    getMainMap.addMouseListener(new MouseListener {
      override def mouseExited(e: MouseEvent): Unit = {}

      override def mouseClicked(e: MouseEvent): Unit = {
        val bounds = getMainMap.getViewportBounds
        for(i <- pixelPositionsOfNodes.indices){
          val point = new Point(pixelPositionsOfNodes(i)._1.toInt - bounds.x, pixelPositionsOfNodes(i)._2.toInt - bounds.y)
          if(point.distance(e.getPoint) < 10) {
            popupPointInformation(i)
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

trait RoutingMapDisplay {
  def drawRoutes(): Unit
}

object RoutingMap{

  def apply(vrp: VRP,
            nodesPositions: List[(scala.Double,scala.Double)],
            vehiclesToColor: Array[Color],
            size: Option[Int] = None,
            displayOnRealMap: Boolean = false): JPanel with RoutingMapDisplay={

    if(displayOnRealMap){
      new GeoRoutingMap(vrp,nodesPositions,vehiclesToColor)
    } else {
      new BasicRoutingMap(vrp,nodesPositions,vehiclesToColor,size.getOrElse(computeSize(nodesPositions)))
    }

  }

  private def computeSize(nodesPositions: List[(scala.Double,scala.Double)]): Int ={
    val distanceBetweenNodes = Array.tabulate(nodesPositions.length)(x =>
      Array.tabulate(nodesPositions.length)(y =>
        List(nodesPositions(x)._1 - nodesPositions(y)._1,nodesPositions(x)._2 - nodesPositions(y)._2))).
      flatten
    val maxDistance = distanceBetweenNodes.flatten.max
    (maxDistance*1.05).toInt
  }
}
