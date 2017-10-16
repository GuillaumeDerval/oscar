package oscar.cbls.business.routing.visual.routingMap

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

import oscar.cbls.business.routing.model.VRP
import oscar.cbls.util.StopWatch


/**
  * @author fabian.germeau@student.vinci.be
  */
class RoutingMapContainer(vrp:VRP,
                          routingMap: JPanel with RoutingMapDisplay,
                          title:String = "Routing map",
                          geolocalisationMap: Boolean = false,
                          routeToDisplay:Boolean = false,
                          refreshRate: Int = 100
                            ) extends JFrame with StopWatch {
  setLayout(new BorderLayout())

  startWatch()
  private var lastRefresh: Long = 0

  def refresh = {
    val currentTime = getWatch
    if(currentTime - lastRefresh >= refreshRate){
      lastRefresh = currentTime
      routingMap.drawRoutes()
    }
  }

  setPreferredSize(new Dimension(960,960))
  add(routingMap, BorderLayout.CENTER)
  pack()
  revalidate()
  setVisible(true)
}
