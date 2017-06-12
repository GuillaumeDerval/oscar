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

import java.awt.{BorderLayout, Color, Dimension}
import javax.swing.{JFrame, JPanel}

import oscar.cbls.business.routing.model.PDP
import oscar.cbls.business.routing.model.VRP


/**
  * @author fabian.germeau@student.vinci.be
  */
class RoutingMatrixContainer(title:String = "Routing map",
                             myVRP:VRP = null,
                             geolocalisationMap: Boolean = false,
                             pickupAndDeliveryPoints: Boolean = false,
                             routeToDisplay:Boolean = false
                            ) extends JFrame with Runnable{
  setLayout(new BorderLayout())

  var routingMap:JPanel with RoutingMap = _

  routingMap = (geolocalisationMap,routeToDisplay) match {
    case(true,true) => new GeoRoutingMap with RouteToDisplay
    case(true,false) => new GeoRoutingMap
    case(false,true) => new BasicRoutingMap(if (pickupAndDeliveryPoints) Some(myVRP.asInstanceOf[PDP]) else None, Some(myVRP)) with RouteToDisplay
    case(false,false) => new BasicRoutingMap(if (pickupAndDeliveryPoints) Some(myVRP.asInstanceOf[PDP]) else None, Some(myVRP))
  }

  if(routeToDisplay)
    routingMap.asInstanceOf[RoutingMap with RouteToDisplay].initRouteToDisplay(this)

  var mustRefresh = false

  var allRoutes:Array[List[Int]] = _

  var routes:List[List[Int]] = Nil

  def run(): Unit ={
    while (true) {
      try {
        Thread.sleep(100)
        if(setMustRefresh(false))
          routingMap.setRouteToDisplay(allRoutes)
      }catch{
        case ie:InterruptedException => return
        case e:Exception => e.printStackTrace()
      }
    }
  }

  def setMustRefresh(toSet:Boolean): Boolean = {
    this.synchronized{
      var res = false
      if(toSet && !mustRefresh)
        mustRefresh = true
      else if(!toSet && mustRefresh) {
        mustRefresh = false
        res = true
      }
      res
    }
  }

  def drawPoints(): Unit ={
    routingMap.drawPoints()
  }

  def setColorValues(colorValues:Array[Color] = null): Unit ={
    routingMap.setColorValues(colorValues)
  }

  def setPointsList(pointsList:List[(Double,Double)]): Unit ={
    routingMap.setPointsList(pointsList)
  }

  def setMapSize(mapSize:Int): Unit ={
    routingMap.setMapSize(mapSize)
  }

  setPreferredSize(new Dimension(960,960))
  add(routingMap, BorderLayout.CENTER)
  pack()
  revalidate()
  setVisible(true)
}
